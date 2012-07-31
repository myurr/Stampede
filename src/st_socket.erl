-module(st_socket).

%% API
-export([listen/1, name/1, accept/1, setopts/2, active_once/1]).

-record(st_socket, {sock = undefined, ssl = false, type = undefined, options = [], name = <<"undefined">>}).

%% ===================================================================
%% API
%% ===================================================================

listen(SocketDet) ->
	Port = proplists:get_value(port, SocketDet, 80),
	IP = proplists:get_value(ip, SocketDet),
	Ssl = proplists:get_bool(ssl, SocketDet),
	do_listen(Ssl, IP, Port, SocketDet).

do_listen(false, IP, Port, SocketDet) ->
	SockOpts = [
		binary,
		{active, false},
		{packet, http_bin},
		{reuseaddr, true},
		{packet_size, 16384},
		{recbuf, 16384},
		{backlog, proplists:get_value(backlog, SocketDet, 100)}
	],

	FinalSockOpts = if IP == undefined -> SockOpts; true -> [{ip, IP} | SockOpts] end,

	Name = if IP == undefined -> <<"*:", (stutil:to_binary(Port))/binary>>;
		true -> <<(stutil:to_binary(IP))/binary, $:, (stutil:to_binary(Port))/binary>> end,

	case gen_tcp:listen(Port, FinalSockOpts) of
		{ok, LSock} ->
			{ok, #st_socket{sock = LSock, ssl = false, type = server, options = SocketDet, name = Name}};
		Err ->
			Err
	end.

name(S) ->
	S#st_socket.name.

accept(ListenerSocket) when ListenerSocket#st_socket.ssl == false ->
	case gen_tcp:accept(ListenerSocket#st_socket.sock) of
		{ok, Socket} ->
			{ok, ListenerSocket#st_socket{sock = Socket}};
		Err ->
			Err
	end.


setopts(Socket, Options) ->
	inet:setopts(Socket#st_socket.sock, Options).

active_once(Socket) ->
	inet:setopts(Socket#st_socket.sock, [{active, once}]).
