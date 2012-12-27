-module(bs_room).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_BINGO_NUMBER, 75).
-define(MAX_WAIT_TIME, 10).
-define(MAX_PLAY_TIME, 40).
-define(TIMER_INTERVAL, 1).
-record(state, {players = [], beholders = [], isbegin, timeline, bingo_number = []}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    timer:send_interval(timer:seconds(?TIMER_INTERVAL),{'$gen_cast',timer}),
    {ok, #state{isbegin = false, timeline = 0}}.

handle_call(check, _From, State) ->
    {reply, {ok, State#state.isbegin}, State};
handle_call({login, User}, _From, State) ->
	Beholders = [User | lists:delete(User, State#state.beholders)],
	{reply, {ok, self()} , State#state{beholders = Beholders}};
handle_call({attend, User}, _From, State) ->
    Players = [User | lists:delete(User, State#state.players)],
    Beholders = lists:delete(User, State#state.beholders),
    {reply, {ok, ?MAX_WAIT_TIME - State#state.timeline} , State#state{beholders = Beholders, players = Players}};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(timer, State) ->
    NewState = change_room_state(State),
    {noreply, NewState};
handle_cast({error, _Content}, State) ->
    {error, State};
handle_cast({say, Content}, State) ->
	broadcast(State#state.players, {room_to_player, Content}),
    {noreply, State};
handle_cast(stop, State) ->
    broadcast(State#state.players, {room_closed, "Leave"}),
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%internal functions

change_room_state(State) ->
    case State#state.isbegin of
        true ->
            case State#state.timeline of 
                Timeline when Timeline > ?MAX_PLAY_TIME -> 
                    gen_server:cast(self(),stop),
                    State#state{timeline = State#state.timeline+1};
                _ -> 
                    change_bingo_play_state(State)
            end;
        _ ->
            case State#state.timeline of
                Timeline when Timeline > ?MAX_WAIT_TIME ->
                    start_bingo_game(State);
                _ ->
                    change_bingo_wait_state(State)
            end
    end.

change_bingo_play_state(State) -> 
    if 
        State#state.timeline rem 5 == 0 ->
            NewBingo = add_a_new_number(State#state.bingo_number), 
            [ New | _ ] = NewBingo,
            broadcast(State#state.players, {new_bingo_number, New}),
            State#state{timeline = State#state.timeline + 1, bingo_number = NewBingo};
        true ->
            State#state{timeline = State#state.timeline + 1}
    end.


add_a_new_number(Bingo) ->
    New = random:uniform(?MAX_BINGO_NUMBER),
    case lists:any(fun (E) -> E == New end, Bingo) of
        true->
            add_a_new_number(Bingo);
        false ->
            [New | Bingo]
    end.

change_bingo_wait_state(State) -> 
    %broadcast(State#state.beholders, {room_wait_state, ?MAX_WAIT_TIME - State#state.timeline}),
    State#state{timeline = State#state.timeline+1}.

start_bingo_game(State) ->
    broadcast(State#state.players, game_begin),
    broadcast(State#state.beholders, leave_this_room),
    State#state{timeline = 0, isbegin = true}.

broadcast(Receivers, Msg) ->
    lists:foreach(fun(E) -> gen_server:cast(E, Msg) end, Receivers).
