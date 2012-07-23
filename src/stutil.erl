-module(stutil).

-export([to_binary/1, timestamp/0]).

timestamp() ->
	{MegaSeconds, Seconds, _MS} = now(),
	(MegaSeconds * 1000000) + Seconds.


to_binary(Input) when is_binary(Input) ->
	Input;
to_binary(Input) when is_list(Input) ->
	iolist_to_binary(Input);
to_binary(Input) when is_integer(Input) ->
	iolist_to_binary(integer_to_list(Input));
to_binary(Input) when is_atom(Input) ->
	atom_to_binary(Input, utf8).

