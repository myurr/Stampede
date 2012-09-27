-module(st_fcgi).

% Exported API
-export([new/1, params/2, stdin/2, stdin_end/1, execute/2, set_timeout/2]).

%% ===================================================================
%% Definitions
%% ===================================================================

-record(st_fcgi, {id = 1, socket = undefined, buffer = <<>>, output = <<>>, error = <<>>, connect_list = [],
                    timeout = 60000}).

-define(FCGI_VERSION, 1).

-define(FCGI_BEGIN_REQUEST,     1).
-define(FCGI_ABORT_REQUEST,     2).
-define(FCGI_END_REQUEST,       3).
-define(FCGI_PARAMS,            4).
-define(FCGI_STDIN,             5).
-define(FCGI_STDOUT,            6).
-define(FCGI_STDERR,            7).
-define(FCGI_DATA,              8).
-define(FCGI_GET_VALUES,        9).
-define(FCGI_GET_VALUES_RESULT, 10).
-define(FCGI_UNKNOWN_TYPE,      11).
-define(FCGI_MAXTYPE,           11).    % Not a typo, has the same code of 11.

-define(FCGI_RESPONDER, 1).


%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new request
%% ====================

new(ServerList) ->
    ConnectList = [ S || {_, S} <- lists:sort([{random:uniform(), Base} || Base <- ServerList])],
    record(#st_fcgi{connect_list = ConnectList}, ?FCGI_BEGIN_REQUEST, <<?FCGI_RESPONDER:16, 0, 0:40>>).


%% ====================
%% Add some parameters
%% ====================

params(FCGI, ParamList) ->
    Data = build_params(ParamList, <<>>),
    record(FCGI, ?FCGI_PARAMS, Data).


%% ====================
%% Provide the body content
%% ====================

stdin(FCGI, Data) when byte_size(Data) > 65535 ->
    NewFCGI = record(FCGI, ?FCGI_STDIN, binary:part(Data, 0, 65535)),
    stdin(NewFCGI, binary:part(Data, 65535, byte_size(Data) - 65535));
stdin(FCGI, Data) ->
    record(FCGI, ?FCGI_STDIN, Data).

stdin_end(FCGI) ->
    record(FCGI, ?FCGI_STDIN, <<>>).


%% ====================
%% Send the request
%% ====================

execute(Request, OrigFCGI) ->
    {ok, FCGI} = open_socket(OrigFCGI),
    ok = gen_tcp:send(FCGI#st_fcgi.socket, FCGI#st_fcgi.buffer),
    case receive_reply(FCGI) of
        {ok, NewFCGI} ->
            if NewFCGI#st_fcgi.error == <<>> ->
                st_response:new_from_data(Request, NewFCGI#st_fcgi.output);
            true ->
                {error, NewFCGI#st_fcgi.error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% ====================
%% Accessors and setters
%% ====================

set_timeout(FCGI, Timeout) ->
    FCGI#st_fcgi{timeout = Timeout}.

%% ===================================================================
%% Support functions
%% ===================================================================

%% ====================
%% Create a record
%% ====================

record(FCGI, Type, Data) ->
    DataLen = byte_size(Data),
    Rec = <<?FCGI_VERSION, Type, (FCGI#st_fcgi.id):16, (byte_size(Data)):16,
            (if DataLen rem 8 == 0 -> 0; true -> 8 - (DataLen rem 8) end),
            0, Data/binary, 0:(if DataLen rem 8 == 0 -> 0; true -> (8 - (DataLen rem 8)) * 8 end)>>,
    FCGI#st_fcgi{buffer = <<(FCGI#st_fcgi.buffer)/binary, Rec/binary>>}.

%% ====================
%% Build up the paramters from a list
%% ====================

build_params([{Key, Value} | Rest], Data) ->
    build_params(Rest,
        <<Data/binary,
        (if byte_size(Key) > 127 -> 1; true -> 0 end):1,                          % key length encoding
        (byte_size(Key)):(if byte_size(Key) > 127 -> 63; true -> 7 end),        % key length value
        (if byte_size(Value) > 127 -> 1; true -> 0 end):1,                        % key length encoding
        (byte_size(Value)):(if byte_size(Value) > 127 -> 63; true -> 7 end),    % key length value
        Key/binary,
        Value/binary>>);
build_params([], Data) ->
    <<Data/binary, 0, 0>>.

%% ====================
%% Open a new socket
%% ====================

open_socket(FCGI) ->
    case FCGI#st_fcgi.connect_list of
        [{IP, Port} | Rest] ->
            case gen_tcp:connect(IP, Port, [binary, {active, true}, {packet, fcgi}]) of
                {ok, Sock} ->
                    {ok, FCGI#st_fcgi{socket = Sock}};
                {error, Reason} ->
                    io:format("FCGI: Couldn't connect to ~p:~p:  ~p~n", [IP, Port, Reason]),
                    open_socket(FCGI#st_fcgi{connect_list = Rest})
            end;
        [] ->
            io:format("FCGI:  No servers to connect to.~n"),
            {error, no_host}
    end.

%% ====================
%% Receive the reply synchronously
%% ====================

receive_reply(FCGI) ->
    Timeout = FCGI#st_fcgi.timeout,
    Socket = FCGI#st_fcgi.socket,
    receive
        {tcp, Socket, Packet} ->
            Id = FCGI#st_fcgi.id,
            <<(?FCGI_VERSION), PacketType, Id:16,
                            Len:16, _PadLen, 0, Payload/binary>> = Packet,
            Data = binary:part(Payload, 0, Len),
            case process_reply_packet(FCGI, PacketType, Data) of
                {ok, NewFCGI} -> receive_reply(NewFCGI);
                {close, NewFCGI} -> 
                    gen_tcp:close(Socket),
                    {ok, NewFCGI#st_fcgi{socket = undefined}}
            end;
        {tcp_closed, Socket} ->
            {ok, FCGI#st_fcgi{socket = undefined}};
        Msg ->
            io:format("FCGI unhandled message: ~p~n", [Msg]),
            receive_reply(FCGI)
    after
        Timeout ->
            io:format("FCGI timed out.~n"),
            {error, timeout}
    end.

process_reply_packet(FCGI, ?FCGI_STDOUT, Data) ->
    {ok, FCGI#st_fcgi{output = <<(FCGI#st_fcgi.output)/binary, Data/binary>>}};

process_reply_packet(FCGI, ?FCGI_STDERR, Data) ->
    io:format("Error Out: ~p~n", [Data]),
    {ok, FCGI#st_fcgi{error = <<(FCGI#st_fcgi.error)/binary, Data/binary>>}};

process_reply_packet(FCGI, ?FCGI_END_REQUEST, _Data) ->
    {close, FCGI}.
