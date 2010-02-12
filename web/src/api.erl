%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(api).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    api_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    api_sup:start_link().

%% @spec start() -> ok
%% @doc Start the api server.
start() ->
    api_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(api).

%% @spec stop() -> ok
%% @doc Stop the api server.
stop() ->
    Res = application:stop(api),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
