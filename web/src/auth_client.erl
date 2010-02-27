-module(auth_client).
-compile([export_all]).
-include_lib("stdlib/include/qlc.hrl").


-record(auth_session, {
        username,
        application,
        token,
        created
       }).

-record(auth_profile, {
        username,
        password
    }).
-record(auth_profiledata, {
        username,
        profile}).


%% Client functions %%

create_db() -> 
    %% Uncomment if used in supervisor

    mnesia:create_schema(config_server:get_value(disc_nodes, [node()])),
    mnesia:start(),

    Profiles = config_server:get_value(profiles_table,
        [
            {ram_copies, [node()]}
        ]) ++ 
        [
            {attributes, record_info(fields, auth_profile)},
            {type, set}
        ],
    Sessions = config_server:get_value(sessions_table,
        [
            {ram_copies, [node()]}
        ]) ++ 
        [   
            {attributes, record_info(fields, auth_session)},
            {type, bag}
        ],
    ProfilesData = config_server:get_value(profiledata_table,
        [
            {disc_only_copies, [node()]}
        ]) ++ 
        [   
            {attributes, record_info(fields, auth_profiledata)},
            {type, set}
        ],
    
    io:format("creating table: ~p~n", [mnesia:create_table(auth_profile, Profiles)]),
    io:format("creating table: ~p~n", [mnesia:create_table(auth_session, Sessions)]),
    io:format("creating table: ~p~n", [mnesia:create_table(auth_profiledata, ProfilesData)]),

    io:format("Mnesia: ~p~n", [mnesia:info()]).


create_profile(Username, Password, Profile) ->
    F = fun() ->
        mnesia:write(#auth_profile{username=Username,
                password=erlang:md5(Password)}),
        mnesia:write(#auth_profiledata{username=Username, profile=Profile})
     
    end,
    {atomic, R} = mnesia:transaction(F),
    R.

change_password(Username, Password) ->
    F = fun() ->
            [ProfileObject] = qlc:eval( qlc:q(
                    [P || P <- mnesia:table(auth_profile), P#auth_profile.username == Username])),
            mnesia:write(ProfileObject#auth_profile{password=erlang:md5(Password)})
    end,
    {atomic, R} = mnesia:transaction(F),
    R.

get_profile(Username) ->
    F = fun() ->
            qlc:eval( qlc:q(
                    [ P#auth_profiledata.profile || P <- mnesia:table(auth_profiledata), P#auth_profiledata.username == Username]))
    end,
    {atomic, [R]} = mnesia:transaction(F),
    R.

delete_profile(Username) ->
    F = fun() ->
            Profiles = qlc:eval( qlc:q(
                    [P || P <- mnesia:table(auth_profile), 
                        P#auth_profile.username == Username])),
            ProfileData = qlc:eval( qlc:q(
                    [P || P <- mnesia:table(auth_profiledata), 
                        P#auth_profiledata.username == Username])),
            lists:foreach(fun(User) ->
                        mnesia:delete_object(User) end, Profiles),
            lists:foreach(fun(User) ->
                        mnesia:delete_object(User) end, ProfileData)
    end,
    mnesia:transaction(F).

update_profile(Username, Profile) ->
    F = fun() ->
            R = qlc:eval( qlc:q(
                    [P || P <- mnesia:table(auth_profiledata), P#auth_profiledata.username == Username])),
            case R of 
                [] ->
                    mnesia:write(#auth_profiledata{username=Username, profile=Profile});
                [ProfileObject] ->
                    mnesia:write(ProfileObject#auth_profiledata{profile=Profile})
            end
    end,
    {atomic, R} = mnesia:transaction(F),
    R.


all_profiles() ->
    F = fun() ->
            qlc:eval( qlc:q( 
                    [P || P <- mnesia:table(auth_profile)]))
    end,
    {atomic, R} = mnesia:transaction(F),
    R.

all_profiledatas() ->
    F = fun() ->
            qlc:eval( qlc:q( 
                    [P || P <- mnesia:table(auth_profiledata)]))
    end,
    {atomic, R} = mnesia:transaction(F),
    R.


all_sessions() ->
    F = fun() ->
            qlc:eval( qlc:q( 
                    [P || P <- mnesia:table(auth_session)]))
    end,
    {atomic, R} = mnesia:transaction(F),
    R.

create_token(Username, Application) ->
    Token = string:strip(os:cmd("uuidgen -t"), right, $\n),
    F = fun() ->
            case mnesia:read(auth_profile, Username) of 
                [] -> ok;
                _Results -> 
                    mnesia:write(
                        #auth_session{username=Username,
                        application=Application,
                        token=Token,
                        created=get_date()})
            end
    end,
    {atomic, ok} = mnesia:transaction(F),
    Token.

match_token(Username, Token, Application) ->
    %% Result = ok || {error, nomatch}
    Expire = config_server:get_value(session_length, 0),
    After = case Expire of 
        0 ->
            0;
        Seconds ->
            get_date() - Seconds
    end,
    io:format("After: ~p~n", [After]),
    io:format("Token: ~p~n", [Token]),
    io:format("Application: ~p~n", [Application]),
    io:format("Username: ~p~n", [Username]),
    io:format("Expire: ~p~n", [Expire]),
    F = fun() ->
            qlc:eval( qlc:q(
                    [S || S <- mnesia:table(auth_session), S#auth_session.username == Username, 
                        S#auth_session.application == Application,
                        S#auth_session.token == Token,
                        S#auth_session.created >= After]))
    end,
    {atomic, R} = mnesia:transaction(F),
    case R of 
        [] ->
            {error, nomatch};
        _ ->
            ok
    end.

clear_tokens(Username, Application) ->
    F = fun() ->
            Sessions = qlc:eval( qlc:q(
                    [S || S <- mnesia:table(auth_session), 
                        S#auth_session.username == Username,
                        S#auth_session.application == Application])),
            lists:foreach(fun(User) ->
                        mnesia:delete_object(User) end, Sessions)
    end,
    mnesia:transaction(F).

get_date() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

get_new_token(Username, Application) ->
    F = fun() ->
            clear_tokens(Username, Application),
            create_token(Username, Application)
    end,
    {atomic, Token} = mnesia:transaction(F),
    Token.

authenticate(Username, Password, Application) ->
    clear_tokens(Username, Application),
    case mnesia:dirty_read(auth_profile, Username) of
        [] -> {error, nouser};
        [User|_] -> 
            Matches = erlang:md5(Password) == User#auth_profile.password,
            if Matches ->
                    {ok, create_token(Username, Application)};
                true ->
                    {error, bad_password}
            end
    end.
