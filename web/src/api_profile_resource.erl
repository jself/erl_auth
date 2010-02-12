%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(api_profile_resource).
-export([init/1, allowed_methods/2, get_profile/2, content_types_provided/2,
    process_post/2, malformed_request/2, delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {{trace, "/tmp"}, []}.
    %%{ok, []}.

is_defined([], _) ->
    true;

is_defined([Value | T ], Plist) ->
    case proplists:is_defined(Value, Plist) of
        false ->
            false;
        true ->
            is_defined(T, Plist)
    end.

get_post(Req, Context) ->
    case proplists:is_defined(data, Context) of
        true ->
            {proplists:get_value(data, Context), Context};
        _ -> 
            Data = mochiweb_util:parse_qs(wrq:req_body(Req)),
            {Data, [{data, Data} | Context]}
    end.

malformed_request(Req, Context) ->
    case wrq:method(Req) of 
        'POST' ->
            {Data, NewC} = get_post(Req, Context),
            {not is_defined(["username", "profile"], Data), Req, NewC};
        'GET' ->
            {not proplists:is_defined("username", wrq:req_qs(Req)), Req, Context};
        'DELETE' ->
            {Data, NewC} = get_post(Req, Context),
            {not is_defined(["username"], Data), Req, NewC}
    end.

allowed_methods(Req, Context) -> 
    {['POST', 'GET', 'DELETE'], Req, Context}.

delete_resource(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    auth_client:delete_profile(Username),
    {true, wrq:append_to_response_body("Deleted", Req), NewC}.

content_types_provided(Req, Context) ->
    %    {[{"application/json", to_json}], Req, Context}.
    % easier to see...
    {[{"text/plain", get_profile}], Req, Context}.

get_profile(Req, Context) ->
    Username = wrq:get_qs_value("username", Req),
     Profile = 
        try auth_client:get_profile(Username) of
            X -> X
        catch 
            error:{badmatch, _} -> []
        end,
    case Profile of
        [] ->
            {"Error: Profile Not Found", Req, Context};
        P ->
            {mochijson2:encode({struct, [{profile, erlang:list_to_binary(P)}]}), Req, Context}
    end.

process_post(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    Profile = proplists:get_value("profile", Data),
    case proplists:is_defined("password", Data) of
        true ->
            %%check the token to see if it's correct
            Password = proplists:get_value("password", Data),
            case auth_client:create_profile(Username, Password, Profile) of
                ok -> {true, wrq:append_to_response_body("Created", Req), NewC};
                R -> {false, wrq:append_to_response_body(io_lib:format("~p", [R]), Req), context}
            end;
        false ->
            case auth_client:update_profile(Username, Profile) of
                ok -> {true, wrq:append_to_response_body("Changed", Req), NewC};
                R -> {false, wrq:append_to_response_body(io_lib:format("~p", [R]), Req), context}
            end
    end.

