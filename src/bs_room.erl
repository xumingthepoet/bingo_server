-module(bs_room).

-behaviour(gen_server).

-include("common.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_ATTEND_PLAYER, 49).
-define(BINGO_LEFT_RATE, 5). %% 2/5
-define(MAX_BINGO_NUMBER, 75).
-define(MAX_WAIT_TIME, 30).
-define(MAX_PLAY_TIME, 350).
-define(TIMER_INTERVAL, 1).
-record(state, {bingoplayersinfo=[], playersinfo=[], players=[], beholders=[], isbegin=false, timeline, bingo_number=[], timer, bingo_left = 0, bingo_rank = 0}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    <<A1:32,A2:32,A3:32>> = crypto:rand_bytes(12),
    random:seed(A1,A2,A3),
    {ok, TRef} = timer:send_interval(timer:seconds(?TIMER_INTERVAL),{'$gen_cast',timer}),
    {ok, #state{isbegin = false, timeline = 0, timer = TRef}}.

handle_call({leave_room, User}, _From, State) ->
    Beholders = lists:delete(User, State#state.beholders),
    Players = lists:delete(User, State#state.players),
    {reply, {ok, User}, State#state{beholders = Beholders, players = Players}};
handle_call(bingo_left_info, _From, State) ->
    {reply, {ok, {State#state.bingo_left, State#state.bingoplayersinfo}}, State};
handle_call({bingo_left, Info}, _From, State) ->
    Result =    if
                    State#state.bingo_left > 0 ->
                        true;
                    true ->
                        false
                end,
    Bingo_left =    if
                        Result == true ->
                            broadcast(State#state.players, {bingo_left, State#state.bingo_left-1}),
                            broadcast(State#state.players, {bingo_info_player, Info}),
                            Bingoplayersinfo = [ Info | State#state.bingoplayersinfo],
                            if
                                State#state.bingo_left == 1 ->
                                    gen_server:cast(self(),stop);
                                true ->
                                    ok
                            end,
                            State#state.bingo_left-1;
                        true ->
                            Bingoplayersinfo = State#state.bingoplayersinfo,
                            State#state.bingo_left
                    end,
    Bingo_rank =    if
                        Result == true ->
                            State#state.bingo_rank+1;
                        true ->
                            State#state.bingo_rank
                    end,
    {reply, {ok, Result, Bingo_rank}, State#state{bingo_left = Bingo_left, bingo_rank = Bingo_rank, bingoplayersinfo = Bingoplayersinfo}};
handle_call(check, _From, State) ->
    R = 
        if 
            State#state.isbegin == true ->
                true;
            true ->
                if 
                    ?MAX_WAIT_TIME - State#state.timeline > 3 ->
                        PlayerNumber = lists:flatlength(State#state.players) ,
                        if 
                            PlayerNumber =< ?MAX_ATTEND_PLAYER ->
                                false;
                            true ->
                                true
                        end;
                    true ->
                        true
                end
        end,
    {reply, {ok, R}, State};
handle_call(check_not_begin, _From, State) ->
    R = 
        if 
            State#state.isbegin == false ->
                if 
                    ?MAX_WAIT_TIME - State#state.timeline >= 1 ->
                        {true, ?MAX_WAIT_TIME - State#state.timeline};
                    true ->
                        {false, 0}
                end;
            true ->
                {false, 0}
        end,
    {reply, {ok, R}, State};
handle_call({login, User}, _From, State) ->
	Beholders = [User | lists:delete(User, State#state.beholders)],
	{reply, {ok, self()} , State#state{beholders = Beholders}};
handle_call({attend, User, Info}, _From, State) ->
    Response =  
        if 
            State#state.isbegin == false ->
                {New, List}  =  case   lists:any(fun(E) -> E == User end, State#state.players) of
                                    true ->
                                        {false, lists:delete(User, State#state.players)};
                                    _ ->
                                        broadcast(State#state.players, {player_info, Info}),
                                        {true, State#state.players}
                                end,
                Players = [User | List],
                Beholders = lists:delete(User, State#state.beholders),
                broadcast(Players, {player_number, lists:flatlength(Players)}),
                Playersinfo = [Info|lists:delete(Info, State#state.playersinfo)],        
                if  New == true ->
                        gen_server:cast(User, {players_info, Playersinfo});
                    true ->
                        gen_server:cast(User, {players_info, State#state.playersinfo})
                end,
                ?MAX_WAIT_TIME - State#state.timeline;
            true ->
                Players = lists:delete(User, State#state.players),
                Beholders = lists:delete(User, State#state.beholders),
                Playersinfo = State#state.playersinfo,
                -1
        end,
    {reply, {ok, Response} , State#state{beholders = Beholders, players = Players ,playersinfo = Playersinfo }};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({leave_room, User}, State) ->
    Beholders = lists:delete(User, State#state.beholders),
    Players = lists:delete(User, State#state.players),
    {noreply, State#state{beholders = Beholders, players = Players}};
handle_cast(timer, State) ->
    NewState = change_room_state(State),
    {noreply, NewState};
handle_cast({error, _Content}, State) ->
    {error, State};
handle_cast({say, Content}, State) ->
	broadcast(State#state.players, {room_to_player, Content}),
    {noreply, State};
handle_cast(stop, State) ->
    broadcast(State#state.players, room_closed),
    {stop, normal, State}.

handle_info(Msg, State) ->
    io:format("room unhandled msg: ~p ~n",[Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    timer:cancel(State#state.timer),
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
    Bingo_left1 = 2 * lists:flatlength(State#state.players) div ?BINGO_LEFT_RATE,
    Bingo_left =
    if 
        Bingo_left1 == 0 ->
            1;
        true ->
            Bingo_left1
    end,
    broadcast(State#state.players, {bingo_left, Bingo_left}),
    State#state{timeline = 0, isbegin = true, bingo_left = Bingo_left}.

broadcast(Receivers, Msg) ->
    lists:foreach(fun(E) -> gen_server:cast(E, Msg) end, Receivers).
