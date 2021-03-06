
-module(stampede_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_listener/6, start_config/2]).

%% Supervisor callbacks
-export([init/1]).

%% Constants
-define(SHUTDOWN_TIME, 10000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listener(ChildId, Socket, RoutingRules, SiteDefinitions, HandlerModule, Options) ->
	ChildCmd = {stampede_listener, start_link, [Socket, RoutingRules, SiteDefinitions, HandlerModule, Options]},
	ChildSpec = {ChildId, ChildCmd, permanent, ?SHUTDOWN_TIME, supervisor, [stampede_listener]},
	supervisor:start_child(?MODULE, ChildSpec).

start_config(ConfigFile, Options) ->
	ChildCmd = {stampede_config, start_link, [ConfigFile, Options]},
	ChildSpec = {stampede_config, ChildCmd, permanent, brutal_kill, worker, [stampede_config]},
	supervisor:start_child(?MODULE, ChildSpec).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	SessionStartCmd = {st_session, start_link, []},
	SessionChildSpec = {st_session, SessionStartCmd, temporary, brutal_kill, worker, [st_session]},
	
	MQStartCmd = {st_mq, start_link, []},
	MQChildSpec = {st_mq, MQStartCmd, temporary, brutal_kill, worker, [st_mq]},
	
	JQSCmd = {st_jqs, start_link, []},
	JQSSpec = {st_jqs, JQSCmd, temporary, brutal_kill, worker, [st_jqs]},
	
	RestartStrategy = {one_for_one, 1000, 10},
	{ok, {RestartStrategy, [SessionChildSpec, MQChildSpec, JQSSpec]}}.


%% ===================================================================
%% Internal functions
%% ===================================================================

