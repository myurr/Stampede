-module(st_routing).

% Exported API
-export([route/2]).

%% ===================================================================
%% Definitions
%% ===================================================================

-record(rst, {path = undefined, base_path = undefined, url_parts = []}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Route a request
%% ====================

route(Request, Rules) ->
	Rst = #rst{url_parts = split_url(st_request:url(Request))},
	rule(Rst, Rules, Request).


%% ===================================================================
%% Rule matching and execution functions
%% ===================================================================

% Out of rules...
rule(_Rst, [], Request) ->
	case st_request:method(Request) of
		'ERROR' ->
			% Default error message
			StatusCode = stutil:to_integer(st_request:arg(Request, <<"error">>, 500)),
			ErrorDetail = stutil:to_binary(st_request:arg(Request, <<"detail">>, <<"Unknown error.">>)),
			{ok, Response} = st_response:new(
					st_request:http_version(Request), StatusCode, [],
					<<"<html><head><title>", (stutil:http_status_code(StatusCode))/binary, "</title></head>",
					"<body><h1>", (stutil:http_status_code(StatusCode))/binary, "</h1><div>",
					ErrorDetail/binary, "</div></body></html>">>,
					false
				),
			{send, Response};

		_ ->
			ErrDetail = <<"Could not find the page: ", (st_request:url(Request))/binary>>,
			{error, 404, ErrDetail}
	end;

% Match the request method
rule(Rst, [{method, Method, SubRules} | Rules], Request) ->
	RMethod = st_request:method(Request),
	if RMethod == Method ->
		rule(Rst, SubRules, Request);
	true ->
		rule(Rst, Rules, Request)
	end;

% Match a host entry
rule(Rst, [{host, Host, SubRules} | Rules], Request) ->
	HostMatch = lists:member(st_request:hostname(Request), stutil:make_list(Host)),
	if HostMatch == true ->
		rule(Rst, SubRules, Request);
	true ->
		rule(Rst, Rules, Request)
	end;

% Set the path
rule(Rst, [{set_path, Path} | Rules], Request) ->
	rule(set_path(Rst, Path), Rules, Request);

% Append to the path
rule(Rst, [{path, Path} | Rules], Request) ->
	rule(append_path(Rst, Path), Rules, Request);

% Match a URL
rule(Rst, [{url, MatchRule, SubRules} | Rules], Request) ->
	case url_match(Rst#rst.url_parts, split_url(MatchRule)) of
		{match, Url} ->
			rule(Rst#rst{url_parts = Url}, SubRules, Request);
		false ->
			rule(Rst, Rules, Request)
	end;

% Match a static directory of content
rule(Rst, [{static_dir, DefaultFile} | Rules], Request) ->
	BaseDir = Rst#rst.path,
	FileName = case Rst#rst.url_parts of
		[] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		[<<>>] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		Parts -> combine_url_path(Parts, BaseDir)
	end,
	case filelib:is_regular(FileName) of
		true ->
			{ok, Response} = st_response:new(
					st_request:http_version(Request), ok, [],
					{file, FileName},
					st_request:keepalive(Request)
				),
			{send, Response};
		false ->
			rule(Rst, Rules, Request)
	end;

% Send some static content
rule(_Rst, [{static, Content} | _Rules], Request) ->
	{ok, Response} = st_response:new(
					st_request:http_version(Request), ok, [],
					Content,
					st_request:keepalive(Request)
				),
	{send, Response};

% Other rule
rule(Rst, [Rule | _Rules], Request) ->
	io:format("Unknown rule~n"),
	{ok, Response} = st_response:new(
			st_request:http_version(Request), 500, [],
			stutil:to_binary(io_lib:format("<pre>Error unknown rule: ~p~n~n~p~n~n~p</pre>", [Rule, Rst, Request])),
			false
		),
	{send, Response};

% Broken call
rule(_Rst, Rules, _Request) ->
	io:format("Bad call to rule() with rules: ~p~n", [Rules]),
	error.

%% ===================================================================
%% Support functions
%% ===================================================================

set_path(Rst, OrigPath) ->
	Path = case binary:last(OrigPath) of
		$/ -> binary:part(OrigPath, 0, byte_size(OrigPath) - 1);
		_ -> OrigPath
	end,
	Rst#rst{path = Path, base_path = Path}.

append_path(Rst, OrigPath) ->
	Path = case binary:last(OrigPath) of
		$/ -> binary:part(OrigPath, 0, byte_size(OrigPath) - 1);
		_ -> OrigPath
	end,
	Rst#rst{path = <<(Rst#rst.path)/binary, $/, Path/binary>>}.

split_url(<<$/, Url/binary>>) ->
	binary:split(Url, <<$/>>, [global]);
split_url(Url) ->
	binary:split(Url, <<$/>>, [global]).

url_match([Path | RestUrl], [Match | RestMatch]) ->
	if Path == Match -> url_match(RestUrl, RestMatch);
	true -> false end;
url_match([], []) ->
	{match, []};
url_match(Url, []) ->
	{match, Url};
url_match([], _Match) ->
	false.


combine_url_path([Part | Rest], Acc) ->
	combine_url_path(Rest, <<Acc/binary, $/, Part/binary>>);
combine_url_path([], Acc) ->
	Acc.
