-module(st_response).

% Exported API
-export([new/1, new/4, set_headers/2, header/3, status_code/1, status_code/2, body/2, filename/2,
		output_response/1, last_modified/2]).


%% ===================================================================
%% Definitions
%% ===================================================================

-include_lib("kernel/include/file.hrl").

-record(st_response, {request = undefined, send_state = buffer,
						status_code = 200, headers = [],
						content_type = <<"text/html">>, 
						body_type = binary, content = <<>>, content_length = undefined,
						last_modified}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new response
%% ====================

new(Request) ->
	{ok, #st_response{request = Request}}.

new(Request, StatusCode, Headers, Body) ->
	{ok, Response} = new(Request),
	{ok, StatusCodeResponse} = status_code(Response, StatusCode),
	{ok, FinalResponse} = set_headers(StatusCodeResponse, Headers),
	case Body of
		{file, FileName} -> filename(FinalResponse, FileName);
		_ -> body(FinalResponse, Body)
	end.


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

status_code(Response) ->
	Response#st_response.status_code.


%% ====================
%% Set the body output
%% ====================

body(Response, Body) when is_binary(Body) ->
	{ok, Response#st_response{body_type = binary, content = Body}}.


%% ====================
%% Set the last modified date
%% ====================

last_modified(Response, Date) ->
	{ok, Response#st_response{last_modified = Date}}.


%% ====================
%% Output a static file
%% ====================

filename(Response, FileName) ->
	{ok, Response#st_response{body_type = file, content = FileName}}.


%% ====================
%% Output the entire body content
%% ====================

output_response(Response) when Response#st_response.body_type == binary ->
	% io:format("Response:~n~p~n~n", [Response]),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10,
				(Response#st_response.content)/binary	>>,
		undefined,
		st_request:keepalive(Response#st_response.request)};

output_response(Response) when Response#st_response.body_type == file ->
	% io:format("Response:~n~p~n~n", [Response]),
	{ok, Fd} = file:open(Response#st_response.content, [read, raw, binary]),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10>>,
		{file, Fd},
		st_request:keepalive(Response#st_response.request)}.


output_http_version(Response) ->
	case st_request:http_version(Response#st_response.request) of
		{1, 1} -> <<"HTTP/1.1">>;
		{1, 0} -> <<"HTTP/1.0">>
	end.


output_headers(Response) when Response#st_response.body_type == file ->
	{ok, FileInfo} = file:read_file_info(Response#st_response.content),
	MTime = FileInfo#file_info.mtime,
	% io:format("Last modified: ~p~n", [MTime]),
	<<(output_headers(Response#st_response.headers, <<>>))/binary,
		(connection_header(Response))/binary, 13, 10,
		"Date: ", (stutil:to_binary(httpd_util:rfc1123_date()))/binary, 13, 10,
		"Last-Modified: ", (stutil:to_binary(httpd_util:rfc1123_date(MTime)))/binary, 13, 10,
		"Content-Length: ", (stutil:to_binary(FileInfo#file_info.size))/binary, 13, 10>>;

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
	byte_size(Response#st_response.content);
content_length(Response) when Response#st_response.body_type == file ->
	filelib:file_size(Response#st_response.content).

connection_header(Response) ->
	case st_request:keepalive(Response#st_response.request) of
		false ->
			<<"Connection: close">>;
		KA ->
			<<"Connection: Keep-Alive", 13, 10,
				"Keep-Alive: timeout=", (stutil:to_binary(KA))/binary>>
	end.
