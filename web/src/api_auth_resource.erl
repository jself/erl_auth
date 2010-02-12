%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(api_auth_resource).
-export([init/1, allowed_methods/2, 
    process_post/2, malformed_request/2]).

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
    {Data, NewC} = get_post(Req, Context),
    {not is_defined(["username", "password", "application"], Data), Req, NewC}.

allowed_methods(Req, Context) -> 
    {['POST','HEAD'], Req, Context}.

process_post(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    Password = proplists:get_value("password", Data),
    Application = proplists:get_value("application", Data),
    case auth_client:authenticate(Username, Password, Application) of
            {ok, Token} -> 
                {true, wrq:append_to_response_body(Token, Req), NewC};
            {error, bad_password} ->
                {true, wrq:append_to_response_body("Error: Bad Password", Req), NewC};
            {error, nouser} ->
                {true, wrq:append_to_response_body("Error: User does not exist", Req), NewC}
    end.

