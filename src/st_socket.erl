-module(st_socket).

%% API
-export([listen/1, port/1, name/1, accept/1, setopts/2, active_once/1, close/1, send/2, send_file/2, recv/3, is_ssl/1, peername/1]).

-record(st_socket, {sock = undefined, port = 0, ssl = false, type = undefined, base_packet = raw, options = [], name = <<"undefined">>}).

%% ===================================================================
%% API
%% ===================================================================

listen(SocketDet) ->
	Port = proplists:get_value(port, SocketDet, 80),
	IP = proplists:get_value(ip, SocketDet),
	Ssl = proplists:get_bool(ssl, SocketDet),
	do_listen(Ssl, IP, Port, SocketDet).

do_listen(false, IP, Port, SocketDet) ->
	BasePacket = case proplists:get_value(data, SocketDet, undefined) of true -> raw; _ -> http_bin end,
	SockOpts = [
		binary,
		{active, false},
		{packet, BasePacket},
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
			{ok, #st_socket{sock = LSock, port = Port, ssl = false, type = server, options = SocketDet, name = Name, base_packet = BasePacket}};
		Err ->
			Err
	end.


port(S) ->
	S#st_socket.port.

name(S) ->
	S#st_socket.name.

accept(ListenerSocket) when ListenerSocket#st_socket.ssl == false ->
	case gen_tcp:accept(ListenerSocket#st_socket.sock) of
		{ok, Socket} ->
			{ok, ListenerSocket#st_socket{sock = Socket}};
		Err ->
			Err
	end.


setopts(Socket, Options) when Socket#st_socket.ssl == false ->
	inet:setopts(Socket#st_socket.sock, Options).

active_once(Socket) when Socket#st_socket.ssl == false ->
	inet:setopts(Socket#st_socket.sock, [{active, once}]).

close(#st_socket{sock = undefined}) ->
	ok;
close(Socket) when Socket#st_socket.ssl == false ->
	gen_tcp:close(Socket#st_socket.sock).

send(_Socket, <<>>) ->
	ok;
send(Socket, Data) when Socket#st_socket.ssl == false ->
	gen_tcp:send(Socket#st_socket.sock, Data).

send_file(Socket, Fd) when Socket#st_socket.ssl == false ->
	file:sendfile(Fd, Socket#st_socket.sock, 0, 0, []).

recv(Socket, Len, Timeout) when Socket#st_socket.ssl == false ->
	inet:setopts(Socket#st_socket.sock, [{packet, raw}]),
	Ret = gen_tcp:recv(Socket#st_socket.sock, Len, Timeout),
	inet:setopts(Socket#st_socket.sock, [{packet, Socket#st_socket.base_packet}]),
	Ret.

is_ssl(Socket) ->
	Socket#st_socket.ssl.

peername(Socket) ->
	inet:peername(Socket#st_socket.sock).

