-module(st_request).

% Exported API
-export([new/4, terminate/1, header/3, end_headers/1, execute/1, error_request/4]).

%% ===================================================================
%% Definitions
%% ===================================================================

-define(DEFAULT_KEEPALIVE, 30).

-record(st_request, {socket = undefined, error = undefined, method, url, args, http_version = {1, 1},
						headers = [], 'Content-Length' = 0, 'Host' = undefined, keepalive = false,
						prev_requests = []}).


%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new request
%% ====================

new(Socket, Method, Path, Version) ->
	{ok, Url, Args} = decode_url(Path),
	io:format("Method ~p, URL ~p, Args ~p, Version ~p~n", [Method, Url, Args, Version]),
	{ok, #st_request{socket = Socket, method = Method, url = Url, args = Args, http_version = Version,
					keepalive = if Version == {1,1} -> ?DEFAULT_KEEPALIVE; true -> false end}}.


%% ====================
%% Terminate a request
%% ====================

terminate(Request) ->
	{ok, Request}.


%% ====================
%% Set a header
%% ====================

header(Request, 'Host', Value) ->
	case binary:split(Value, <<$:>>) of
		[Host, Port] ->
			{ok, Request#st_request{'Host' = {Host, Port}}};
		[Host] ->
			{ok, Request#st_request{'Host' = {Host, st_socket:port(Request#st_request.socket)}}}
	end;

header(Request, 'Content-Length', Value) ->
	{ok, Request#st_request{'Content-Length' = stutil:to_integer(Value)}};

header(Request, 'Connection', Value) ->
	case stutil:bstr_to_lower(Value) of
		<<"keep-alive">> ->
			{ok, Request#st_request{keepalive = ?DEFAULT_KEEPALIVE}};
		<<"close">> ->
			{ok, Request#st_request{keepalive = false}};
		_ ->
			{ok, Request#st_request{headers = [{'Connection', Value} | Request#st_request.headers]}}
	end;

header(Request, Key, Value) ->
	{ok, Request#st_request{headers = [{Key, Value} | Request#st_request.headers]}}.

%% ====================
%% End of the headers
%% ====================

end_headers(Request) ->
	io:format("Request:~n~p~n~n", [Request]),
	{ok, Request}.


%% ====================
%% Execute a request
%% ====================

execute(Request) ->
	{ok, Response} = st_response:new(http_version(Request), 200, [], <<"Hello World">>, Request#st_request.keepalive),
	{send, Response}.


%% ====================
%% Create a new error request
%% ====================

error_request(State, StatusCode, Error, Reason) ->
	State#st_request{method = 'Error', url = stutil:to_binary(StatusCode), 
						args = [{<<"error">>, stutil:to_binary(Error)}, {<<"reason">>, stutil:to_binary(Reason)}],
						headers = [],
						prev_requests = [State | State#st_request.prev_requests]}.


%% ===================================================================
%% Internal functions
%% ===================================================================

decode_url(Path) ->
	case binary:split(Path, <<$?>>) of
		[Url] ->
			{ok, Url, []};
		[Url, Args] ->
			{ok, Url, decode_url_args(binary:split(Args, <<$&>>, [global]), [])}
	end.

decode_url_args([Arg | Rest], ArgList) ->
	case binary:split(Arg, <<$=>>) of
		[Key] ->
			decode_url_args(Rest, [{stutil:urldecode(Key), <<>>} | ArgList]);
		[Key, Value] ->
			decode_url_args(Rest, [{stutil:urldecode(Key), stutil:urldecode(Value)} | ArgList])
	end;
decode_url_args([], ArgList) ->
	ArgList.

http_version(Request) ->
	Request#st_request.http_version.
