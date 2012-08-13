-module(st_request).

% Exported API
-export([new/4, terminate/1, header/3, end_headers/1, execute/2, error_request/4,
		http_version/1, method/1, url/1, host/1, hostname/1, arg/2, arg/3, keepalive/1,
		if_modified_since/1]).

%% ===================================================================
%% Definitions
%% ===================================================================

-define(DEFAULT_KEEPALIVE, 30).

-record(st_request, {socket = undefined, error = undefined, method, url, args, http_version = {1, 1},
						headers = [], content_length = 0, host = undefined, keepalive = false,
						if_modified_since = undefined,
						prev_requests = []}).


%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new request
%% ====================

new(Socket, Method, Path, Version) ->
	{ok, Url, Args} = decode_url(Path),
	% io:format("Method ~p, URL ~p, Args ~p, Version ~p~n", [Method, Url, Args, Version]),
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
			{ok, Request#st_request{host = {Host, Port}}};
		[Host] ->
			{ok, Request#st_request{host = {Host, st_socket:port(Request#st_request.socket)}}}
	end;

header(Request, 'Content-Length', Value) ->
	{ok, Request#st_request{content_length = stutil:to_integer(Value)}};

header(Request, 'Connection', Value) ->
	case stutil:bstr_to_lower(Value) of
		<<"keep-alive">> ->
			{ok, Request#st_request{keepalive = ?DEFAULT_KEEPALIVE}};
		<<"close">> ->
			{ok, Request#st_request{keepalive = false}};
		_ ->
			{ok, Request#st_request{headers = [{'Connection', Value} | Request#st_request.headers]}}
	end;

header(Request, 'If-Modified-Since', Value) ->
	{ok, Request#st_request{if_modified_since = httpd_util:convert_request_date(binary_to_list(Value))}};

header(Request, Key, Value) ->
	{ok, Request#st_request{headers = [{Key, Value} | Request#st_request.headers]}}.

%% ====================
%% End of the headers
%% ====================

end_headers(Request) ->
	% io:format("Request:~n~p~n~n", [Request]),
	{ok, Request}.


%% ====================
%% Execute a request
%% ====================

execute(Request, RoutingRules) ->
	st_routing:route(Request, RoutingRules).



%% ====================
%% Create a new error request
%% ====================

error_request(Request, StatusCode, Error, Detail) ->
	Request#st_request{method = 'ERROR', url = stutil:to_binary(StatusCode), 
						args = [{<<"error">>, stutil:to_binary(Error)}, {<<"detail">>, stutil:to_binary(Detail)}],
						headers = [],
						prev_requests = [Request | Request#st_request.prev_requests]}.


%% ====================
%% Accessors
%% ====================

http_version(Request) ->
	Request#st_request.http_version.

method(Request) ->
	Request#st_request.method.

url(Request) ->
	Request#st_request.url.

host(Request) ->
	Request#st_request.host.

hostname(Request) ->
	{Host, _Port} = Request#st_request.host,
	Host.

arg(Request, Key) ->
	proplists:get_value(Key, Request#st_request.args).

arg(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.args, Default).

keepalive(Request) ->
	% false.
	Request#st_request.keepalive.

if_modified_since(Request) ->
	Request#st_request.if_modified_since.

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

