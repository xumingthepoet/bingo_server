-module(bs_room).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_BINGO_NUMBER, 75).
-define(MAX_WAIT_TIME, 1).
-define(MAX_PLAY_TIME, 1).
-record(state, {players, beholders, isbegin, timeline, bingo_number}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    bs_engine:register_listener(self()),
    {ok, #state{players = [], beholders = [], isbegin = false, timeline = 0}}.

handle_call(check, _From, State) ->
    {reply, {ok, State#state.isbegin}, State};
handle_call({login, Content}, _From, State) ->
	Players = [Content | State#state.players],
	{reply, {ok, self()} , State#state{players = Players}};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(engine, State) ->
    NewState = change_room_state(State),
    {noreply, NewState};
handle_cast(game_begin, State) ->
    {noreply , State#state{isbegin = true, timeline = 0}};
handle_cast({error, _Content}, State) ->
    {error, State};
handle_cast({say, Content}, State) ->
	broadcast(State#state.players, {room_to_player, Content}),
    {noreply, State};
handle_cast(stop, State) ->
    broadcast(State#state.players, {room_to_player, "Leave"}),
    bs_engine:unregister_listener(self()),
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
                    gen_server:cast(self(),game_begin),
                    State#state{timeline = 0};
                _ ->
                    change_bingo_wait_state(State)
            end
    end.

change_bingo_play_state(State) -> 
    NewBingo = add_a_new_number(State#state.bingo_number), 
    [ New | _ ] = NewBingo,
    broadcast(State#state.players, {new_bingo_number, New}),
    State#state{timeline = State#state.timeline + 1, bingo_number = NewBingo}.

add_a_new_number(Bingo) ->
    New = random:uniform(75),
    IsContain = lists:any(fun (E) -> E == New end,Bingo),
    if IsContain ->
        add_a_new_number(Bingo);
    true ->
        [New | Bingo]
    end.

change_bingo_wait_state(State) -> State#state{timeline = State#state.timeline+1}.

broadcast(Receivers, Msg) ->
    lists:foreach(fun(E) -> gen_server:cast(E, Msg) end, Receivers).
