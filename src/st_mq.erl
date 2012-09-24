-module(st_mq).

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Exported API
-export([create_stream/2, subscribe/1, subscribe/2, subscribe/3, unsubscribe/1, unsubscribe/2, 
			unsubscribe_all/1, unsubscribe_all/0,
			async_send_message/3, send_message/2, send_message/3,
			msg_stream/1, msg_data/1, msg_timestamp/1]).

%% ===================================================================
%% Definitions
%% ===================================================================

-record(mqstate, {process_check_time}).

-record(stmq_msg, {stream_id, data, timestamp}).

-record(stmq_stream, {id, pool, pool_size}).

-record(stmq_subscribe, {id, pid}).

-define(PROCESS_CHECK_FREQUENCY, 300).


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

start_link() ->
	init_mq(),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	State = #mqstate{process_check_time = stutil:timestamp() + ?PROCESS_CHECK_FREQUENCY},
	{ok, State, 0}.

handle_call(Request, From, State) ->
	io:format("Unexpected call to st_mq: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State, timeout(State)}.

handle_cast(Request, State) ->
	io:format("Unexpected cast to st_mq: ~p, ~p~n", [Request, State]),
	{noreply, State, timeout(State)}.

handle_info({'EXIT', Pid, _Reason}, State) ->
	unsubscribe_all(Pid),
	{noreply, State, timeout(State)};
handle_info(timeout, State) ->
	{noreply, State#mqstate{process_check_time = stutil:timestamp() + ?PROCESS_CHECK_FREQUENCY}};

handle_info(Msg, State) ->
	io:format("Unexpected info to st_mq: ~p, ~p~n", [Msg, State]),
	{noreply, State, timeout(State)}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State, timeout(State)}.


%% ===================================================================
%% Accessors
%% ===================================================================

msg_stream(Msg) ->
	Msg#stmq_msg.stream_id.

msg_data(Msg) ->
	Msg#stmq_msg.data.

msg_timestamp(Msg) ->
	Msg#stmq_msg.timestamp.


%% ===================================================================
%% API functions
%% ===================================================================

create_stream(StreamId, Options) ->
	PoolSize = proplists:get_value(pool_size, Options, 0),
	Stream = #stmq_stream{id = StreamId, pool = queue:new(), pool_size = PoolSize},
	mnesia:activity(transaction, fun() ->
		case mnesia:read(stmq_stream, StreamId) of
			[] ->
				mnesia:write(stmq_stream, Stream, write),
				io:format("Created st_mq stream ~p~n", [Stream]),
				ok;
			_ ->
				{error, stream_exists}
		end
	end).

subscribe(StreamId) ->
	subscribe(StreamId, [{send_pool, true}], self()).

subscribe(StreamId, Options) ->
	subscribe(StreamId, Options, self()).

subscribe(StreamId, Options, Pid) ->
	mnesia:activity(async_dirty, fun() ->
		mnesia:write(stmq_subscribe, #stmq_subscribe{id = StreamId, pid = Pid}, write),
		link(whereis(?MODULE))
	end),
	SendPool = proplists:get_value(send_pool, Options),
	if SendPool == true ->
		Pool = mnesia:activity(async_dirty, fun() ->
			case mnesia:read(stmq_stream, StreamId) of
				[] ->
					[];
				[Stream] ->
					queue:to_list(Stream#stmq_stream.pool)
			end
		end),
		send_messages_to_pid(Pool, Pid);
	true -> ok end.


unsubscribe(StreamId) ->
	unsubscribe(StreamId, self()).

unsubscribe(StreamId, Pid) ->
	mnesia:activity(async_dirty, fun() ->
		mnesia:delete_object(stmq_subscribe, #stmq_subscribe{id = StreamId, pid = Pid}, write),
		unlink(whereis(?MODULE))
	end).

unsubscribe_all() ->
	unsubscribe_all(self()).

unsubscribe_all(Pid) ->
	Subs = mnesia:activity(async_dirty, fun() ->
		mnesia:match_object(stmq_subscribe, #stmq_subscribe{id = '_', pid = Pid}, read)
	end),
	unsubscribe_list(Subs).

unsubscribe_list([Sub | Rest]) ->
	mnesia:activity(async_dirty, fun() ->
		mnesia:delete_object(stmq_subscribe, Sub, write)
	end),
	unsubscribe_list(Rest);
unsubscribe_list([]) ->
	ok.

async_send_message(StreamId, Data, Options) ->
	spawn(st_mq, send_message, [StreamId, Data, Options]).

send_message(StreamId, Data) ->
	send_message(StreamId, Data, []).

send_message(StreamId, Data, Options) ->
	Msg = #stmq_msg{stream_id = StreamId, data = Data, timestamp = stutil:timestamp()},

	% Update the stream pool
	SkipPool = proplists:get_value(skip_pool, Options),
	if SkipPool == true ->
		mnesia:activity(transaction, fun() ->
			case mnesia:read(stmq_stream, StreamId) of
				[] ->
					{error, stream_not_found};
				[Stream] ->
					PoolSize = Stream#stmq_stream.pool_size,
					if PoolSize > 0 ->
						NewPool = queue:in(Msg, Stream#stmq_stream.pool),
						QueueLen = queue:len(NewPool),
						FinalPool = if
							QueueLen > PoolSize -> {_Discard, P} = queue:out(NewPool), P;
							true -> NewPool
						end,
						mnesia:write(stmq_stream, Stream#stmq_stream{pool = FinalPool}, write);
					true ->
						ok
					end
			end
		end);
	true -> ok end,

	% Get the list of subscribed PIDs
	Pids = mnesia:activity(async_dirty, fun() ->
		mnesia:read(stmq_subscribe, StreamId)
	end),

	% Broadcast the data to each subscriber
	send_message_to_pids({st_mq, Msg}, [Sub#stmq_subscribe.pid || Sub <- Pids]).


%% ===================================================================
%% Internal functions
%% ===================================================================

init_mq() ->
	io:format("Initialising st_mq.~n"),
	{atomic, ok} = mnesia:create_table(stmq_stream, [
									{attributes, record_info(fields, stmq_stream)},
									{type, set}, 
									{record_name, stmq_stream}
								]),
	{atomic, ok} = mnesia:create_table(stmq_subscribe, [
									{attributes, record_info(fields, stmq_subscribe)},
									{type, bag}, 
									{index, [#stmq_subscribe.pid]}, 
									{record_name, stmq_subscribe}
								]),
	io:format("Initialisation of st_mq complete.~n"),
	ok.

timeout(State) ->
	Delta = State#mqstate.process_check_time - stutil:timestamp(),
	if Delta >= 0 -> Delta;
		true -> 0 end.

send_message_to_pids(Msg, [Pid | Pids]) ->
	Pid ! Msg,
	send_message_to_pids(Msg, Pids);
send_message_to_pids(_Msg, []) ->
	ok.

send_messages_to_pid([Msg | Pool], Pid) ->
	Pid ! Msg,
	send_messages_to_pid(Pool, Pid);
send_messages_to_pid([], _Pid) ->
	ok.


