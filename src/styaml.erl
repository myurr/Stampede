-module(styaml).

-export([decode/1, encode/1]).

-record(parser, {state, line, col, offset}).

-define(INC_LINE(S), S#parser{line = S#parser.line + 1, col = 1, offset = S#parser.offset + 1}).
-define(INC_COL(S, N), S#parser{col = S#parser.col + byte_size(<<N/utf8>>), offset = S#parser.offset + byte_size(<<N/utf8>>)}).
-define(INC_COL_STR(S, N), S#parser{col = S#parser.col + byte_size(N), offset = S#parser.offset + byte_size(N)}).
-define(IS_WHITESPACE(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_CHAR(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z))).
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_NAME(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Str) ->
	try
		parse(Str)
	catch
		throw:{error, Msg, S} ->
			{error, stutil:to_binary(io_lib:format("Error '~s' on line ~p, column ~p.", [Msg, S#parser.line, S#parser.col]))};

		Err:Det ->
			{error, stutil:to_binary(io_lib:format("Error: ~p:~p~n    ~p", [Err, Det, erlang:get_stacktrace()]))}
	end.

encode(_Struct) ->
	<<"">>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tokenise and decode a YAML file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Str) ->
	parse(Str, #parser{line = 1, col = 1, offset = 0, state = any}).

parse(Str, State) ->
	case tokenise(Str, State) of
		{string, String, _S} ->
			String;
		% {property, PropName, S} ->
		% 	parse_property(Str, PropName, S);
		Ret ->
			io:format("Tokenise returned ~p~n", [Ret]),
			throw_error(io_lib:format("Unexpected object encountered ~p", [Ret]), State)
	end.


tokenise(Str, S = #parser{offset = Offset}) ->
	case Str of
		<<_:Offset/binary, $", _/binary>> ->
			tokenise_string(Str, ?INC_COL(S, $"), <<>>);
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_CHAR(C) ->
			tokenise_property(Str, ?INC_COL(S, C), <<C>>);
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_WHITESPACE(C) ->
			if S#parser.state == properties ->
				tokenise_whitespace(Str, S, 0);
			true ->
				NewState = skip_whitespace(Str, ?INC_COL(S, C)),
				tokenise(Str, NewState)
			end;
		<<_:Offset/binary>> ->
			{eof, S};
		_ ->
			throw_error(<<"unexpected character">>, S)
	end.

skip_whitespace(Str, S = #parser{offset = Offset}) ->
	case Str of
		<<_:Offset/binary, $\n, _/binary>> ->
			skip_whitespace(Str, ?INC_LINE(S));
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_WHITESPACE(C) ->
			skip_whitespace(Str, ?INC_COL(S, C));
		_ ->
			S
	end.

tokenise_whitespace(Str, S = #parser{offset = Offset}, Len) ->
	case Str of
		<<_:Offset/binary, $\n, _/binary>> ->
			{blank_line, S};
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_WHITESPACE(C) ->
			tokenise_whitespace(Str, ?INC_COL(S, C), Len + 1);
		_ ->
			{whitespace, Len, S}
	end.

tokenise_string(Str, S = #parser{offset = Offset}, Acc) ->
	case Str of
		<<_:Offset/binary, $", _/binary>> ->
			{string, Acc, ?INC_COL(S, $")};
		<<_:Offset/binary, $\\, C/utf8, _/binary>> ->
			tokenise_string(Str, ?INC_COL_STR(S, <<$\\, C/utf8>>), <<Acc/binary, C/utf8>>);
		<<_:Offset/binary, C/utf8, _/binary>> ->
			tokenise_string(Str, ?INC_COL(S, C), <<Acc/binary, C/utf8>>)
	end.


tokenise_property(Str, S = #parser{offset = Offset}, Acc) ->
	case Str of
		<<_:Offset/binary, $:, _/binary>> ->
			{property, Acc, ?INC_COL(S, $:)};
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_NAME(C) ->
			tokenise_property(Str, ?INC_COL(S, C), <<Acc/binary, C/utf8>>);
		_ ->
			io:format("Str is ~p, S is ~p, Acc is ~p~n", [Str, S, Acc]),
			throw_error(<<"unexpected character in property name">>, S)
	end.

throw_error(Msg, S) ->
	throw({error, Msg, S}).
