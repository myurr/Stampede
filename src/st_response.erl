-module(st_response).

% Exported API
-export([new/1, new/4, new/2, new_from_data/2, 
		set_headers/2, header/3, status_code/1, status_code/2, body/2, filename/2, stream/2, websocket/1,
		output_response/1, last_modified/2, request/1, encode_chunk/1, last_chunk/1, get_header/2, get_header/3,
		body_type/1, save_to_file/2, calc_etag/1, content_type/2]).


%% ===================================================================
%% Definitions
%% ===================================================================

-include_lib("kernel/include/file.hrl").

-record(st_response, {request = undefined, send_state = buffer,
						status_code = 200, headers = [],
						content_type = undefined, 
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
	{ok, HeadersResponse} = set_headers(StatusCodeResponse, Headers),
	{ok, FinalResponse} = set_session_headers(HeadersResponse, Request),
	case Body of
		{file, FileName} -> filename(FinalResponse, FileName);
		{stream, FirstChunk} -> stream(FinalResponse, FirstChunk);
		_ -> body(FinalResponse, Body)
	end.

new(Request, CI) ->
	Status = st_cache:status_code(CI),
	if Status == 200; Status == ok ->
		MatchEtag = st_request:if_none_match(Request),
		ResponseEtag = st_cache:etag(CI),
		if MatchEtag == ResponseEtag ->
			{ok, Response} = new(Request),
			{ok, StatusCodeResponse} = status_code(Response, not_modified),
			{ok, FinalResponse} = set_session_headers(StatusCodeResponse, Request),
			body(FinalResponse, <<>>);
		true ->
			BaseHeaders = [{<<"Etag">>, ResponseEtag}],
			Headers = case st_cache:content_type(CI) of
				undefined -> BaseHeaders;
				Type -> [{<<"Content-Type">>, Type} | BaseHeaders]
			end,
			{ok, Response} = new(Request),
			{ok, StatusCodeResponse} = status_code(Response, Status),
			{ok, HeadersResponse} = set_headers(StatusCodeResponse, Headers),
			{ok, FinalResponse} = set_session_headers(HeadersResponse, Request),
			filename(FinalResponse, st_cache:cache_file(CI))
		end;
	true ->
		{error, Status, st_cache:error_detail(CI)}
	end.

new_from_data(Request, Data) ->
	{ok, Response} = new(Request),
	{ok, StatusCodeResponse} = status_code(Response, ok),
	{ok, FinalResponse} = set_session_headers(StatusCodeResponse, Request),
	decode_data(FinalResponse, Request, Data).

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

get_header(Response, Key) ->
	proplists:get_value(Key, Response#st_response.headers).

get_header(Response, Key, Default) ->
	proplists:get_value(Key, Response#st_response.headers, Default).



%% ====================
%% Content type
%% ====================

content_type(Response, ContentType) ->
	Response#st_response{content_type = ContentType}.

%% ====================
%% Set session cookies
%% ====================

set_session_headers(Response, Request) ->
	% io:format("Setting session: ~p~n", [st_request:session(Request)]),
	case st_request:session(Request) of
		undefined ->
			{ok, Response};
		Session ->
			set_headers(Response, st_session:get_response_headers(Session))
	end.


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

body(Response, Body) ->
	{ok, Response#st_response{body_type = binary, content = stutil:to_binary(Body)}}.



%% ====================
%% Output a static file
%% ====================

filename(Response, FileName) ->
	{ok, Response#st_response{body_type = file, content = FileName}}.


%% ====================
%% Streaming output
%% ====================

stream(Response, Body) ->
	{ok, Response#st_response{body_type = stream, content = stutil:to_binary(Body)}}.

%% ====================
%% Set up a web socket
%% ====================

websocket(Response) ->
	{ok, Response#st_response{body_type = websocket, content = <<>>}}.

%% ====================
%% Set the last modified date
%% ====================

last_modified(Response, Date) ->
	{ok, Response#st_response{last_modified = Date}}.


%% ====================
%% Retrieve the body type
%% ====================

body_type(Response) ->
	Response#st_response.body_type.

%% ====================
%% Save the response output to a file
%% ====================

save_to_file(Response, FileName) when Response#st_response.body_type == binary ->
	filelib:ensure_dir(FileName),
	ok = file:write_file(FileName, Response#st_response.content);
save_to_file(Response, FileName) when Response#st_response.body_type == file ->
	filelib:ensure_dir(FileName),
	{ok, _Bytes} = file:copy(Response#st_response.content, FileName),
	ok;
save_to_file(Response, _FileName) when Response#st_response.body_type == stream ->
	{error, bad_body_type}.

%% ====================
%% Calculate an etag for the response
%% ====================

calc_etag(Response) when Response#st_response.body_type == binary ->
	<<$", (base64:encode(crypto:sha(Response#st_response.content)))/binary, $">>;
calc_etag(Response) when Response#st_response.body_type == file ->
	Context = crypto:sha_init(),
	{ok, IODevice} = file:open(Response#st_response.content, [read, binary, raw]),
	FinalContext = etag_scan_file(IODevice, Context),
	<<$", (base64:encode(crypto:sha_final(FinalContext)))/binary, $">>.

etag_scan_file(IODevice, Context) ->
	case file:read(IODevice, 4194304) of
		{ok, Data} ->
			etag_scan_file(IODevice, crypto:sha_update(Data));
		_ ->
			Context
	end.

%% ====================
%% Output the entire body content
%% ====================

output_response(Response) when Response#st_response.body_type == binary ->
	% io:format("Response:~n~p~n~n", [Response]),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10,
				(Response#st_response.content)/binary>>,
		undefined,
		st_request:keepalive(Response#st_response.request)};

output_response(Response) when Response#st_response.body_type == stream ->
	% io:format("Response:~n~p~n~n", [Response]),
	FirstChunk = encode_chunk(Response#st_response.content),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10,
				(FirstChunk)/binary	>>,
		undefined,
		stream};

output_response(Response) when Response#st_response.body_type == websocket ->
	% io:format("Response:~n~p~n~n", [Response]),
	{ok, <<		(output_http_version(Response))/binary, $ ,
				(stutil:http_status_code(Response#st_response.status_code))/binary,
				13, 10, 
				(output_headers(Response))/binary, 13, 10>>,
		undefined,
		websocket};

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
	ContentType = if Response#st_response.content_type == undefined -> <<>>;
		true -> <<"Content-type: ", (stutil:to_binary(Response#st_response.content_type))/binary>> end,
	<<(output_headers(Response#st_response.headers, <<>>))/binary,
		(connection_header(Response))/binary, 13, 10,
		ContentType/binary,
		"Date: ", (stutil:to_binary(httpd_util:rfc1123_date()))/binary, 13, 10,
		"Last-Modified: ", (stutil:to_binary(httpd_util:rfc1123_date(MTime)))/binary, 13, 10,
		"Content-Length: ", (stutil:to_binary(FileInfo#file_info.size))/binary, 13, 10>>;

output_headers(Response) when Response#st_response.body_type == stream ->
	ContentType = if Response#st_response.content_type == undefined -> <<>>;
		true -> <<"Content-type: ", (stutil:to_binary(Response#st_response.content_type))/binary>> end,
	<<(output_headers(Response#st_response.headers, <<>>))/binary,
		(connection_header(Response))/binary, 13, 10,
		ContentType/binary,
		"Date: ", (stutil:to_binary(httpd_util:rfc1123_date()))/binary, 13, 10,
		"Transfer-Encoding: chunked", 13, 10>>;

output_headers(Response) when Response#st_response.body_type == websocket ->
	output_headers(Response#st_response.headers, <<>>);

output_headers(Response) ->
	ContentType = if Response#st_response.content_type == undefined -> <<>>;
		true -> <<"Content-type: ", (stutil:to_binary(Response#st_response.content_type))/binary>> end,
	<<(output_headers(Response#st_response.headers, <<>>))/binary,
		(connection_header(Response))/binary, 13, 10,
		ContentType/binary,
		"Date: ", (stutil:to_binary(httpd_util:rfc1123_date()))/binary, 13, 10,
		"Content-Length: ", (stutil:to_binary(content_length(Response)))/binary, 13, 10>>.


output_headers([{Key, Value} | Rest], Out) ->
	output_headers(Rest,
		<<Out/binary, (stutil:to_binary(Key))/binary, $:, $ , (stutil:to_binary(Value))/binary, 13, 10>>);
output_headers([], Out) ->
	Out.

content_length(Response) when Response#st_response.body_type == binary ->
	byte_size(Response#st_response.content);
content_length(Response) when Response#st_response.body_type == stream ->
	0;
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

request(Response) ->
	Response#st_response.request.


encode_chunk(undefined) ->
	<<>>;
encode_chunk(<<>>) ->
	<<>>;
encode_chunk(OrigData) ->
	Data = stutil:to_binary(OrigData),
	if byte_size(Data) > 0 ->
		LenStr = iolist_to_binary(integer_to_list(byte_size(Data), 16)),
		<<LenStr/binary, 13, 10, Data/binary, 13, 10>>;
	true ->	<<>>
	end.

last_chunk(AdditionalHeaders) ->
	<<$0, 13, 10, (output_headers(AdditionalHeaders, <<>>))/binary, 13, 10>>.


decode_data(Response, Request, Data) ->
	case erlang:decode_packet(httph_bin, Data, []) of
		{ok, {http_header, _, Key, _, Value}, Rest} ->
			{ok, NewResponse} = header(Response, Key, Value),
			decode_data(NewResponse, Request, Rest);
		{ok, http_eoh, Rest} ->
			body(Response, Rest);
		Unknown ->
			io:format("Unknown packet: ~p~n", [Unknown]),
			{error, bad_data}
	end.
