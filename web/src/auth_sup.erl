-module(auth_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    R = supervisor:start_link(?MODULE, []),
    auth_client:create_db(),
    R.

init([]) ->
    %% Result = {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}} | ignore
    %% RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
    %% MaxR = MaxT = int()>=0
    %% ChildSpec = child_spec()

    %% child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
    %% Id = term()  
    %% StartFunc = {M,F,A} 
    %% M = F = atom()
    %% A = [term()] 
    %% Restart = permanent | transient | temporary
    %% Shutdown = brutal_kill | int()>=0 | infinity 
    %% Type = worker | supervisor
    %% Modules = [Module] | dynamic
    %% VModule = atom()

    {ok, {{one_for_all, 5, 30},
        [
            {config_server, {config_server, start_link, []},
                permanent, 5000, worker, [config_server]}
        ]
    }}.

