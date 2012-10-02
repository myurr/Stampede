-module(styaml).

-export([decode/1, encode/1]).

-record(parser, {state, line, col, offset}).

-define(INC_LINE(S, N), S#parser{line = S#parser.line + 1, col = 1, offset = S#parser.offset + N}).
-define(INC_COL(S, N), S#parser{col = S#parser.col + byte_size(<<N/utf8>>), offset = S#parser.offset + byte_size(<<N/utf8>>)}).
-define(INC_COL_STR(S, N), S#parser{col = S#parser.col + byte_size(N), offset = S#parser.offset + byte_size(N)}).
-define(INC_COL_N(S, N), S#parser{col = S#parser.col + N, offset = S#parser.offset + N}).
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
		{string, String, S} ->
			{eof, _} = tokenise(Str, S),
			String;
		{integer, Num, S} ->
			{eof, _} = tokenise(Str, S),
			Num;
		{float, Num, S} ->
			{eof, _} = tokenise(Str, S),
			Num;
		% {property, PropName, S} ->
		% 	parse_property(Str, PropName, S);
		Ret ->
			io:format("Tokenise returned ~p~n", [Ret]),
			throw_error(io_lib:format("Unexpected object encountered ~p", [Ret]), State)
	end.


tokenise(Str, S = #parser{offset = Offset}) ->
	io:format("Tokenise called at: ~p~n", [binary:part(Str, Offset, byte_size(Str) - Offset)]),
	case Str of
		<<_:Offset/binary, $", _/binary>> ->
			tokenise_string(Str, ?INC_COL(S, $"), <<>>);
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_DIGIT(C) ->
			tokenise_number(Str, ?INC_COL(S,C), <<C>>, integer);
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
		<<_:Offset/binary, "\r\n", _/binary>> ->
			skip_whitespace(Str, ?INC_LINE(S, 2));
		<<_:Offset/binary, $\r, _/binary>> ->
			skip_whitespace(Str, ?INC_LINE(S, 1));
		<<_:Offset/binary, $\n, _/binary>> ->
			skip_whitespace(Str, ?INC_LINE(S, 1));
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_WHITESPACE(C) ->
			skip_whitespace(Str, ?INC_COL(S, C));
		_ ->
			S
	end.

tokenise_whitespace(Str, S = #parser{offset = Offset}, Len) ->
	case Str of
		<<_:Offset/binary, "\r\n", _/binary>> ->
			{blank_line, ?INC_LINE(S, 2)};
		<<_:Offset/binary, $\r, _/binary>> ->
			{blank_line, ?INC_LINE(S, 1)};
		<<_:Offset/binary, $\n, _/binary>> ->
			{blank_line, ?INC_LINE(S, 1)};
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

tokenise_eol_string(Str, S = #parser{offset = Offset}, Acc) ->
	case Str of
		<<_:Offset/binary, "\r\n", _/binary>> ->
			{string, Acc, ?INC_LINE(S, 2)};
		<<_:Offset/binary, "\r", _/binary>> ->
			{string, Acc, ?INC_LINE(S, 1)};
		<<_:Offset/binary, "\n", _/binary>> ->
			{string, Acc, ?INC_LINE(S, 1)};
		<<_:Offset/binary, $\\, C/utf8, _/binary>> ->
			tokenise_eol_string(Str, ?INC_COL_STR(S, <<$\\, C/utf8>>), <<Acc/binary, C/utf8>>);
		<<_:Offset/binary, C/utf8, _/binary>> ->
			tokenise_eol_string(Str, ?INC_COL(S, C), <<Acc/binary, C/utf8>>);
		<<_:Offset/binary>> ->
			{string, Acc, S}
	end.


tokenise_number(Str, S = #parser{offset = Offset}, Acc, Type) ->
	case Str of
		<<_:Offset/binary, $., _/binary>> when Type == integer ->
			tokenise_number(Str, ?INC_COL(S, $.), <<Acc/binary, $.>>, float);
		<<_:Offset/binary, C/utf8, _/binary>> when ?IS_DIGIT(C) ->
			tokenise_number(Str, ?INC_COL(S, C), <<Acc/binary, C/utf8>>, Type);
		<<_:Offset/binary, "\r\n", _/binary>> ->
			{Type, convert_number(Acc, Type), ?INC_LINE(S, 2)};
		<<_:Offset/binary, $\r, _/binary>> ->
			{Type, convert_number(Acc, Type), ?INC_LINE(S, 1)};
		<<_:Offset/binary, $\n, _/binary>> ->
			{Type, convert_number(Acc, Type), ?INC_LINE(S, 1)};
		<<_:Offset/binary>> ->
			{Type, convert_number(Acc, Type), S};
		_ ->
			tokenise_eol_string(Str, S, Acc)
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

convert_number(Str, integer) ->
	list_to_integer(binary_to_list(Str));
convert_number(Str, float) ->
	list_to_float(binary_to_list(Str)).
