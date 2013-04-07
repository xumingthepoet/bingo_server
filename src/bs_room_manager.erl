-module(bs_room_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([find_room/0]).

-record(state, {current_room, shut = false}).

find_room() ->
	{ok, Pid} = gen_server:call(?MODULE,find),
	Pid.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
	register(?MODULE,self()),
	{ok, Room} = bs_room_sup:start_child(),
    {ok, #state{current_room = Room}}.

handle_call(find, _From, State) ->
	Current_room = State#state.current_room,
	if is_pid(Current_room) ->
			Alive = util:is_process_alive(Current_room);
		true ->
			Alive = false
	end,
	if 	Alive ->
			{ok, Isbegin} = 
				try gen_server:call(Current_room,check)
				catch
					_ ->
						{ok, true}
				end;
		true ->	
			Isbegin = true
	end,
    %%io:format("bs_player_handle_cast_attend: ~p.~n" , [Isbegin]),
    bs_log:info("bs_player_handle_cast_attend: ~p.~n" , [Isbegin]),
	case {Isbegin, State#state.shut} of
		{_, true} ->
			{reply, {ok, undefined}, State};
		{false, false} ->
			{reply, {ok, State#state.current_room}, State};
		{_, false} ->
			{ok, NewRoom} = bs_room_sup:start_child(),
			bs_log:info("New Room Created!"),
			{reply, {ok, NewRoom}, #state{current_room = NewRoom}}
	end;
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(change_on, State) ->
    {noreply, State#state{shut=false} };
handle_cast(change_off, State) ->
    {noreply, State#state{shut=true} };
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
