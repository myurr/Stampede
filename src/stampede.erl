-module(stampede).

%% Application helper functions
-export([start/0, listen/4, cmd_listen/3, nodes/1]).

%% ===================================================================
%% Application helper functions
%% ===================================================================

start() ->
	ok = application:start(stampede),
	stampede_sup:start_config("/etc/stampede.conf", []),
	ok.


%% ===================================================================
%% Listener helper functions
%% ===================================================================

listen(SocketDet, RoutingRules, SiteDefinitions, Options) ->
	case st_socket:listen(SocketDet) of
		{ok, ServerSocket} ->
			Id = proplists:get_value(id, Options, ServerSocket),
			stampede_sup:start_listener({stampede_listener, Id}, ServerSocket, RoutingRules, SiteDefinitions, stampede_transport, Options);
		Err ->
			Err
	end.

cmd_listen(SocketDet, SiteDefinitions, Options) ->
	case st_socket:listen([{data, true} | SocketDet]) of
		{ok, ServerSocket} ->
			Id = proplists:get_value(id, Options, ServerSocket),
			stampede_sup:start_listener({stampede_listener, Id}, ServerSocket, undefined, SiteDefinitions, stampede_cmd, Options);
		Err ->
			Err
	end.


%% ===================================================================
%% Initialise stampede, connect it to other nodes
%% ===================================================================

nodes(_NodeList) ->
	ok = stampede_site:init(),
	ok.


