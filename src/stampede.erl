-module(stampede).

%% Application helper functions
-export([start/0]).
-export([listen/3]).

%% ===================================================================
%% Application helper functions
%% ===================================================================

start() ->
	application:start(stampede).



%% ===================================================================
%% Listener helper functions
%% ===================================================================

listen(SocketDet, RoutingRules, Options) ->
	{ok, Socket} = st_socket:listen(SocketDet),
	stampede_sup:start_listener(Socket, RoutingRules, Options).

