%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(api_password_resource).
-export([init/1, allowed_methods/2, 
    process_post/2, malformed_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {{trace, "/tmp"}, []}.
    %%{ok, []}.


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
    {not lib:is_defined(["username", "password"], Data), Req, NewC}.

allowed_methods(Req, Context) -> 
    {['POST','HEAD'], Req, Context}.

process_post(Req, Context) ->
    {Data, NewC} = get_post(Req, Context),
    Username = proplists:get_value("username", Data),
    Password = proplists:get_value("password", Data),
    case auth_client:change_password(Username, Password) of
            ok -> 
                {true, wrq:append_to_response_body("Changed", Req), NewC};
            R -> {false, wrq:append_to_response_body(io_lib:format("~p", [R]), Req), NewC}
    end.

