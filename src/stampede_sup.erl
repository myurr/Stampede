
-module(stampede_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listener(Protocol, IP, HandlerRules, Options) ->
%	{ok, Socket} = open_server_socket(Protocol, IP),
	io:format("Opened the socket~n").
%	ChildSpec = {}
%	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


%% ===================================================================
%% Internal functions
%% ===================================================================

