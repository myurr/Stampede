-module(st_response).

% Exported API
-export([new/1, new/5, set_headers/2, header/3, status_code/2, keepalive/2, output_response/1]).

%% ===================================================================
%% Definitions
%% ===================================================================

-record(st_response, {send_state = buffer, http_version = {1, 1},
						status_code = 200, headers = [],
						content_type = <<"text/html">>, 
						body_type = binary, content = <<>>, content_length = undefined,
						keepalive = false}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new response
%% ====================

new(HttpVersion) ->
	KeepAlive = case HttpVersion of
		{1, 1} -> 30;
		{1, 0} -> false
	end,
	{ok, #st_response{http_version = HttpVersion, keepalive = KeepAlive}}.

new(HttpVersion, StatusCode, Headers, Body, KeepAlive) ->
	{ok, Response} = new(HttpVersion),
	{ok, StatusCodeResponse} = status_code(Response, StatusCode),
	{ok, HeaderResponse} = set_headers(StatusCodeResponse, Headers),
	{ok, KeepAliveResponse} = keepalive(HeaderResponse, KeepAlive),
	body(KeepAliveResponse, Body).


%% ====================
%% Add a header
%% ====================

set_headers(Response, [{Key, Value} | HeadersList]) ->
	case header(Response, Key, Value) of
		{ok, NewResponse} ->
			set_headers(NewResponse, HeadersList);
		Error ->
			Error
	end;
set_headers(Response, []) ->
	{ok, Response}.

header(Response, Key, Value) ->
	{ok, Response#st_response{headers = [{Key, Value} | Response#st_response.headers]}}.


%% ====================
%% Set status code
%% ====================

status_code(Response, StatusCode) ->
	{ok, Response#st_response{status_code = StatusCode}}.


%% ====================
%% Set keepalive policy
%% ====================

keepalive(Response, Timeout) when is_integer(Timeout) ->
	{ok, Response#st_response{keepalive = Timeout}};
keepalive(Response, false) ->
	{ok, Response#st_response{keepalive = false}};
keepalive(_Response, Error) ->
	{error, badarg, Error}.


%% ====================
%% Set the body output
%% ====================

body(Response, Body) when is_binary(Body) ->
	{ok, Response#st_response{body_type = binary, content = Body}}.


%% ====================
%% Output the entire body content
%% ====================

output_response(Response) when Response#st_response.body_type == binary ->
	io:format("Response:~n~p~n~n", [Response]),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10,
				(Response#st_response.content)/binary	>>,
		Response#st_response.keepalive}.

output_http_version(Response) ->
	case Response#st_response.http_version of
		{1, 1} -> <<"HTTP/1.1">>;
		{1, 0} -> <<"HTTP/1.0">>
	end.

output_headers(Response) ->
	<<(output_headers(Response#st_response.headers, <<>>))/binary,
		(connection_header(Response))/binary, 13, 10,
		"Date: ", (stutil:to_binary(httpd_util:rfc1123_date()))/binary, 13, 10,
		"Content-Length: ", (stutil:to_binary(content_length(Response)))/binary, 13, 10>>.

output_headers([{Key, Value} | Rest], Out) ->
	output_headers(Rest,
		<<Out/binary, (stutil:to_binary(Key))/binary, $:, $ , (stutil:to_binary(Value))/binary, 13, 10>>);
output_headers([], Out) ->
	Out.

content_length(Response) when Response#st_response.body_type == binary ->
	byte_size(Response#st_response.content).

connection_header(Response) when Response#st_response.keepalive == false ->
	<<"Connection: close">>;
connection_header(Response) ->
	<<"Connection: Keep-Alive", 13, 10,
		"Keep-Alive: timeout=", (stutil:to_binary(Response#st_response.keepalive))/binary>>.
