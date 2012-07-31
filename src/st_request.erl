-module(st_request).

% Exported API
-export([new/3, header/3, end_headers/2]).

%% ===================================================================
%% Definitions
%% ===================================================================

-define(DEFAULT_KEEPALIVE, 30000).

-record(st_request, {error = undefined, method, path, http_version,
						headers = [], 'Content-Length' = 0, 'Host' = undefined, keepalive = undefined}).


%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new request
%% ====================

new(Method, Path, Version) ->
	io:format("Method ~p, Path ~p, Version ~p~n", [Method, Path, Version]),
	{ok, #st_request{method = Method, path = Path, http_version = Version}}.


%% ====================
%% Set a header
%% ====================

header(Request, 'Host', Value) ->
	{ok, Request#st_request{'Host' = binary:split(Value, <<$:>>)}};

header(Request, 'Content-Length', Value) ->
	{ok, Request#st_request{'Content-Length' = stutil:to_integer(Value)}};

header(Request, 'Connection', Value) ->
	case stutil:bstr_to_lower(Value) of
		<<"keep-alive">> ->
			{ok, Request#st_request{keepalive = ?DEFAULT_KEEPALIVE}};
		<<"close">> ->
			{ok, Request#st_request{keepalive = 0}};
		_ ->
			{ok, Request#st_request{headers = [{'Connection', Value} | Request#st_request.headers]}}
	end;

header(Request, Key, Value) ->
	{ok, Request#st_request{headers = [{Key, Value} | Request#st_request.headers]}}.

%% ====================
%% Set a header
%% ====================

end_headers(Request, _Socket) ->
	io:format("Request:~n~p~n~n", [Request]),
	{ok, Request}.

%% ===================================================================
%% Internal functions
%% ===================================================================

