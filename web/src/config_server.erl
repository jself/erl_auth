-module(config_server).
-behaviour(gen_server). 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get_value/1, get_value/2, get_state/0]).

%% Client functions %%

start_link() -> 
    %% Uncomment if used in supervisor
    %process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_value(Key) ->
    gen_server:call(?MODULE, {get_value, Key}).

get_value(Key, Default) ->
    gen_server:call(?MODULE, {get_value, Key, Default}).

get_state() ->
    gen_server:call(?MODULE, {get_state}).

%% gen_server functions %%

init([]) ->
    %% Result = {ok, State} | {ok, State, Timeout} | {ok, State, hibernate}
    {ok, Settings} = file:consult("settings.cfg"),
    {ok, Settings}.
 

handle_call({get_state}, _From, State) ->
    {reply, State, State};

handle_call({get_value, Key}, _From, State) ->
    {reply, proplists:get_value(Key, State), State};

handle_call({get_value, Key, Default}, _From, State) ->
    {reply, proplists:get_value(Key, State, Default), State};

handle_call(_Request, _From, State) ->
    %% Result = {reply, Reply, NewState} | {reply, Reply, NewState, Timeout}
    %% {reply, Reply, NewState, hibernate}
    %% {noreply, NewState} | {noreply, NewState, Timeout}
    %% {noreply, NewState, hibernate}
    %% {stop, Reason, Reply, NewState} | {stop, Reason, NewState}
    {reply, unknown, State}.

handle_cast(_Msg, State) -> 
    %% Result = {noreply, NewState} | {noreply, NewState, Timeout}
    %% {noreply, NewState, hibernate}
    %% {stop, Reason, NewState}
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Internal functions %%

