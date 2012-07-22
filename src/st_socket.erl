-module(st_socket).

%% API
-export([listen/1]).

-record(st_socket, {sock = undefined, ssl = false, type = undefined, options = []}).

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
		{backlog, 100}
	],

	FinalSockOpts = if IP == undefined -> SockOpts; true -> [{ip, IP} | SockOpts] end,

	case gen_tcp:listen(Port, FinalSockOpts) of
		{ok, LSock} ->
			{ok, #st_socket{sock = LSock, ssl = false, type = server, options = SocketDet}};
		Err ->
			Err
	end.

