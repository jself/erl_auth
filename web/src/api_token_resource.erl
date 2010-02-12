%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(api_token_resource).
-export([init/1, allowed_methods/2, 
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
            {not is_defined(["username", "application"], Data), Req, NewC};
        'DELETE' ->
            {Data, NewC} = get_post(Req, Context),
            {not is_defined(["username", "application"], Data), Req, NewC}
    end.

allowed_methods(Req, Context) -> 
    {['POST', 'DELETE'], Req, Context}.

delete_resource(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    Application = proplists:get_value("application", Data),
    auth_client:clear_tokens(Username, Application),
    {true, wrq:append_to_response_body("Cleared", Req),NewC}.


process_post(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    Application = proplists:get_value("application", Data),
    case proplists:is_defined("token", Data) of
        true ->
            %%check the token to see if it's correct
            Token = proplists:get_value("token", Data),
            case auth_client:match_token(Username, Token, Application) of 
                ok ->
                    {true, wrq:append_to_response_body("Matched", Req), NewC};
                {error, nomatch} ->
                    {true, wrq:append_to_response_body("Error: Invalid Token", Req), NewC};
                E ->
                    {true, 
                        wrq:append_to_response_body(io_lib:format("Error: ~p",[E]), Req),
                     NewC}
            end;
        false ->
            {true, wrq:append_to_response_body(auth_client:create_token(Username, Application), Req), NewC}
    end.

