-module(bs_player).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([login/1, attend/1]).

-define(MAX_BINGO_NUMBER, 75).
-record(state, {tcp, room = room_closed, card1 = [], card2 = [], board = []}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).  

init([]) -> 
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),   
    {ok, #state{}}.

handle_call({check_bingo, []}, _From, State) ->
    {reply, {ok, false}, State};
handle_call({check_bingo, CLpairs}, _From, State) ->
    Check_Result = check_Result(CLpairs, State),
    {reply, {ok, Check_Result}, State};
handle_call(login, From, _State) ->
    Room = bs_room_manager:find_room(),
    gen_server:call(Room, {login, self()}),
    {Pid, _} = From,
    {reply, {ok, login_success}, #state{tcp = Pid, room = Room}};
handle_call(attend, _From , State) ->
    {ok, Time2Begin} = gen_server:call(State#state.room, {attend, self()}),
    Sign1 = "attend:",
    Signn = "\n",
    to_client(State, lists:append([Sign1, integer_to_list(Time2Begin), Signn])),
    {reply, {ok, attend_success}, State};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(bingo_state, _From, State) -> 
    check_all_bingo_result(State),
    {reply, {ok, bingo_state}, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(room_closed, State) ->
    {noreply, State#state{room = room_closed}};
handle_cast(leave_this_room, State) ->
    Room = bs_room_manager:find_room(),
    gen_server:call(Room, {login, self()}),
    {noreply, State#state{ room = Room}};
handle_cast(game_begin, State) ->
    Card1 = generate_a_new_card(),
    Card2 = generate_a_new_card(),
    Sign1 = "game_begin:",
    Sign2 = "and",
    Signn = "\n",
    to_client(State, lists:append([Sign1, integerlist_to_list(Card1), Sign2, integerlist_to_list(Card2), Signn])),
    {noreply, State#state{card1 = Card1, card2 = Card2}};
handle_cast({room_wait_state, TimeRest}, State) ->
    Sign1 = "room_wait_state:",
    Signn = "\n",
    to_client(State, [ Sign1, integer_to_list(TimeRest), Signn]),
    {noreply, State};
handle_cast({say, Content}, State) ->
    io:format("say:~p~n",[Content]),
    to_room(State, {say,Content}),
    {noreply, State};
handle_cast({room_to_player,Content}, State) ->
    io:format("reply:~p~n",[Content]),
    to_client(State, {room_to_player, Content}),
    {noreply, State};
handle_cast({new_bingo_number,Num}, State) ->
    Sign1 = "new_number:",
    Signn = "\n",
    to_client(State, lists:append([Sign1, integer_to_list(Num), Signn])),
    {noreply, State#state{board = [ Num | State#state.board ] }};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%non-behaviourial APIs
login(UID) ->
    case bs_data:is_alive(UID) of
        true ->
            bs_data:get_alive_player_pid(UID);
        _ ->
            {ok,Player} = bs_player_sup:start_child(),
            {ok,_} = gen_server:call(Player, login),
            Player
    end.

attend(Pid) ->
    gen_server:call(Pid, attend).

%%internal functions

to_client(State, Msg) ->
    if is_pid(State#state.tcp) ->
        case erlang:is_process_alive(State#state.tcp) of 
            true ->
                gen_server:cast(State#state.tcp, {to_client, Msg});
            _ -> 
                null
        end;
    true ->
        null
    end.

to_room(State ,Msg) ->
    if is_pid(State#state.room) ->
        case erlang:is_process_alive(State#state.room) of 
            true ->
                gen_server:cast(State#state.room, {to_room, Msg});
             _ -> 
                null
        end;
    true ->
        null
    end.

generate_a_new_card() ->
    Col1 = generate_x_different_number(5, [], 0),
    Col2 = generate_x_different_number(5, [], 15),
    Col3 = generate_x_different_number(5, [], 30),
    Col4 = generate_x_different_number(5, [], 45),
    Col5 = generate_x_different_number(5, [], 60),
    Col1 ++ Col2 ++ Col3 ++ Col4 ++ Col5.

generate_x_different_number(0, Result, _) ->
    Result;
generate_x_different_number(X, Result, Extra) ->
    New = random:uniform(?MAX_BINGO_NUMBER div 5) + Extra,
    case lists:any(fun(E) -> E == New end, Result) of
        true ->
            generate_x_different_number(X, Result, Extra);
        false ->
            generate_x_different_number(X-1, [New | Result], Extra)
    end.

check_Result([] , _) ->
    true;
check_Result({Card_order, Line_order}, State) ->
    case Card_order of 
        1 ->
            check_Result_(Line_order, State#state.card1, State#state.board);
        2 ->
            check_Result_(Line_order, State#state.card2, State#state.board);
        _ -> 
            false
    end;
check_Result([{Card_order, Line_order}| Rest ], State) ->
    case check_Result({Card_order, Line_order}, State) of
        true ->
            check_Result(Rest, State);
        false ->
            false
    end.

check_Result_(Line_order, Card, Board) ->
    TheLine = 
    case Line_order of 
        1 ->
            [lists:nth(1,Card)]++[lists:nth(6,Card)]++[lists:nth(11,Card)]++[lists:nth(16,Card)]++[lists:nth(21,Card)];
        2 ->
            [lists:nth(2,Card)]++[lists:nth(7,Card)]++[lists:nth(12,Card)]++[lists:nth(17,Card)]++[lists:nth(22 ,Card)];
        3 ->
            [lists:nth(3,Card)]++[lists:nth(8,Card)]++[lists:nth(18,Card)]++[lists:nth(23,Card)];
        4 ->
            [lists:nth(4,Card)]++[lists:nth(9,Card)]++[lists:nth(14,Card)]++[lists:nth(19,Card)]++[lists:nth(24,Card)];
        5 ->
            [lists:nth(5,Card)]++[lists:nth(10,Card)]++[lists:nth(15,Card)]++[lists:nth(20,Card)]++[lists:nth(25,Card)];
        6 ->
            [lists:nth(1,Card)]++[lists:nth(2,Card)]++[lists:nth(3,Card)]++[lists:nth(4,Card)]++[lists:nth(5,Card)];
        7 ->
            [lists:nth(6,Card)]++[lists:nth(7,Card)]++[lists:nth(8,Card)]++[lists:nth(9,Card)]++[lists:nth(10,Card)];
        8 ->
            [lists:nth(11,Card)]++[lists:nth(12,Card)]++[lists:nth(14,Card)]++[lists:nth(15,Card)];
        9 ->
            [lists:nth(16,Card)]++[lists:nth(17,Card)]++[lists:nth(18,Card)]++[lists:nth(19,Card)]++[lists:nth(20,Card)];
        10 ->
            [lists:nth(21,Card)]++[lists:nth(22,Card)]++[lists:nth(23,Card)]++[lists:nth(24,Card)]++[lists:nth(25,Card)];
        11 ->
            [lists:nth(5,Card)]++[lists:nth(9,Card)]++[lists:nth(17,Card)]++[lists:nth(21,Card)];
        12 ->
            [lists:nth(1,Card)]++[lists:nth(7,Card)]++[lists:nth(19,Card)]++[lists:nth(25,Card)];
        13 ->
            [lists:nth(1,Card)]++[lists:nth(5,Card)]++[lists:nth(21,Card)]++[lists:nth(25,Card)];
        _ ->
            null
    end,
    case TheLine of
        null ->
            false;
        _ ->
            contains(TheLine, Board)
    end.

contains([], _) ->
    true;
contains([H|R], List2) ->
    case lists:member(H, List2) of
        true ->
            contains(R, List2);
        false ->
            false
    end.

check_all_bingo_result(State) ->
    io:format("~n~p ",[check_Result({1, 1}, State)]),
    io:format("~p ",[check_Result({1, 2}, State)]),
    io:format("~p ",[check_Result({1, 3}, State)]),
    io:format("~p ",[check_Result({1, 4}, State)]),
    io:format("~p ",[check_Result({1, 5}, State)]),
    io:format("~p ",[check_Result({1, 6}, State)]),
    io:format("~p ",[check_Result({1, 7}, State)]),
    io:format("~p ",[check_Result({1, 8}, State)]),
    io:format("~p ",[check_Result({1, 9}, State)]),
    io:format("~p ",[check_Result({1, 10}, State)]),
    io:format("~p ",[check_Result({1, 11}, State)]),
    io:format("~p ",[check_Result({1, 12}, State)]),
    io:format("~p~n",[check_Result({1, 13}, State)]),
    io:format("~p ",[check_Result({2, 1}, State)]),
    io:format("~p ",[check_Result({2, 2}, State)]),
    io:format("~p ",[check_Result({2, 3}, State)]),
    io:format("~p ",[check_Result({2, 4}, State)]),
    io:format("~p ",[check_Result({2, 5}, State)]),
    io:format("~p ",[check_Result({2, 6}, State)]),
    io:format("~p ",[check_Result({2, 7}, State)]),
    io:format("~p ",[check_Result({2, 8}, State)]),
    io:format("~p ",[check_Result({2, 9}, State)]),
    io:format("~p ",[check_Result({2, 10}, State)]),
    io:format("~p ",[check_Result({2, 11}, State)]),
    io:format("~p ",[check_Result({2, 12}, State)]),
    io:format("~p~n",[check_Result({2, 13}, State)]),
    ok.

integerlist_to_list(Input) -> integerlist_to_list(Input, []).

integerlist_to_list([], Result) -> Result;
integerlist_to_list([H|T], []) -> integerlist_to_list(T, integer_to_list(H));
integerlist_to_list([H|T], Result) -> integerlist_to_list(T, lists:append([integer_to_list(H),[$,], Result])).
