-module(bs_player).

-behaviour(gen_server).

-include("common.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([leave_room/1,attend/3, bingo/2]).

-define(MAX_BINGO_NUMBER, 75).

-record(state, {info, bet, card, tcp, uid, room = room_closed, card1 = [], card2 = [], board = []}).

start_link(INFO) ->
    UID = INFO#userinfo.uid,
    gen_server:start_link({global, {?PROCESS, UID}},?MODULE, [INFO], []).  

init([INFO]) -> 
    UID = INFO#userinfo.uid,
    <<A1:32,A2:32,A3:32>> = crypto:rand_bytes(12),
    random:seed(A1,A2,A3),   
    {ok, #state{info=INFO, uid=UID}, 180000}.

handle_call('GAMES', _From, State) ->
    {reply, {ok, State#state.room}, State};
handle_call('RESET_SOCKET', From, State) ->
    if 
        State#state.tcp /= undefined ->
            gen_server:cast(State#state.tcp, stop);
        true ->
            do_nothing
    end,
    Response =  if
                    State#state.room /= room_closed ->
                        case util:is_process_alive(State#state.room) of
                            true -> 
                                {ok, {NotBegin, TimeLeft}} = gen_server:call(State#state.room, check_not_begin),
                                case NotBegin of 
                                    true ->
                                        {timeleft, TimeLeft};
                                    false ->
                                        {ok, {BingoLeft, BingoLeftInfo}} = gen_server:call(State#state.room, bingo_left_info),
                                        {
                                            BingoLeft,
                                            BingoLeftInfo,
                                            integerlist_to_list(State#state.card1),
                                            integerlist_to_list(State#state.card2),
                                            integerlist_to_list(State#state.board)
                                        }
                                end;      
                            false ->
                                room_closed
                        end;
                    true ->
                        no_data
                end,
    {Pid, _} = From,
    {reply, {ok, Response}, State#state{tcp = Pid}};
handle_call('REGISTER_SOCKET', From, State) ->
    {Pid, _} = From,
    {reply, ok, State#state{tcp = Pid}};
handle_call(change_tcp, From, State) ->
    {Pid, _} = From,
    {reply, ok, State#state{tcp = Pid}};
handle_call(query_player_state, _From, State) ->
    {reply, {ok, State#state.room, State#state.uid}, State};
handle_call({check_bingo, []}, _From, State) ->
    {reply, {ok, false}, State};
handle_call({check_bingo, CLpairs}, _From, State) ->
    Check_Result = check_Result(CLpairs, State),
    {reply, {ok, Check_Result}, State};
handle_call({login, UID}, From, _State) ->
    Room = bs_room_manager:find_room(),
    gen_server:call(Room, {login, self()}),
    {Pid, _} = From,
    {reply, {ok, login_success}, #state{tcp = Pid, uid = UID, room = Room }};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(bingo_state, _From, State) -> 
    check_all_bingo_result(State),
    {reply, {ok, bingo_state}, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(leave_room, State) ->
    case State#state.room  of
        room_closed ->
            ok;
        Pid when is_pid(Pid) ->
            gen_server:call(State#state.room, {leave_room, self()});
        _ ->
            ok
    end,
    {noreply, State#state{room = room_closed}};
handle_cast({bingo_info_player, Content}, State) ->
    bingo_info_to_client(State, Content),
    {noreply, State};
handle_cast({players_info, Content}, State) ->
    info_to_client(State, Content),
    {noreply, State};
handle_cast({player_info, Content}, State) ->
    info_to_client_(State, Content),
    {noreply, State};
handle_cast({bingo_left, Number}, State) ->
    to_client(State, "{\"api\":\"bingo_left\",\"content\":"++integer_to_list(Number)++"}\n"),
    {noreply, State};
handle_cast({player_number, Number}, State) ->
    to_client(State, "{\"api\":\"player_number\",\"content\":"++integer_to_list(Number)++"}\n"),
    {noreply, State};
handle_cast(dont_die, State) ->
    {noreply, State};
handle_cast(tcp_closed, State) ->
    {noreply, State, 60000};
handle_cast({bingo, CLpairs}, State) ->
    { Check_Result, Rank1, Dif1} = 
    try 
        if 
            State#state.room /= room_closed ->
                R = check_Result(CLpairs, State),
                if
                    R == true ->
                        {ok, RR, Rank} = gen_server:call(State#state.room, {bingo_left, State#state.info}),
                        if
                            RR == true ->
                                Diff = State#state.bet div 10 * (21 - Rank),
                                if  Diff > 0 ->
                                        Dif = Diff + State#state.bet;
                                    true ->
                                        Dif = State#state.bet
                                end,
                                bs_data:add_player_money(State#state.uid,  Dif),
                                { true, Rank, Dif};
                            true ->
                                { false, 0, 0}
                        end;
                    true ->
                        { false, 0, 0}
                end;
            true ->
                { false, 0 ,0 }
        end
    catch
        _:_->
            { false, 0 , 0}
    end,
    Info = State#state.info#userinfo{balance = list_to_binary(integer_to_list(list_to_integer(binary_to_list(State#state.info#userinfo.balance))+ Dif1))},
    {Type, _} = CLpairs, 
    to_client(State, "{\"api\":\"bingo\",\"type\":" ++ integer_to_list(Type) ++ ",\"rank\":" ++ integer_to_list(Rank1) ++ ",\"winnings\":" ++ integer_to_list(Dif1) ++ ",\"content\":"++atom_to_list(Check_Result)++"}\n"),
    {noreply, State#state{info = Info}};
handle_cast({attend, Bet, Card} , State) ->
    % R = case util:is_process_alive(State#state.room) of   
    %         true -> 
    %             {ok, Time2Begin2} = gen_server:call(State#state.room, {attend, self(), State#state.info}),
    %             io:format("bs_player_handle_cast_attend:~n"),
    %             if 
    %                 Time2Begin2 > 0 ->
    %                     Time2Begin = Time2Begin2,
    %                     io:format("bs_player_handle_cast_attend2:~n"),
    %                     State#state.room;
    %                 true ->
    %                     gen_server:call(State#state.room, {leave_room, self()}),
    %                     Room = bs_room_manager:find_room(),
    %                     gen_server:call(Room, {login, self()}),
    %                     {ok, Time2Begin} = gen_server:call(Room, {attend, self(), State#state.info}),
    %                     io:format("bs_player_handle_cast_attend3:~n"++pid_to_list(State#state.room)),
    %                     Room
    %             end;
    %         _ ->
    %             Room = bs_room_manager:find_room(),
    %             gen_server:call(Room, {login, self()}),
    %             {ok, Time2Begin} = gen_server:call(Room, {attend, self(), State#state.info}),
    %             io:format("bs_player_handle_cast_attend4:~n"),
    %             Room
    %     end,
    Balance = list_to_integer(binary_to_list(State#state.info#userinfo.balance)),
    Money_enough  = if 
                        Bet*Card =< Balance ->
                            true;
                        true ->
                            false
                    end,
    case util:is_process_alive(State#state.room) of
        true ->
            gen_server:call(State#state.room, {leave_room, self()});
        _ ->
            ok
    end,
    Room1 = if 
                Money_enough == true ->
                    Room = bs_room_manager:find_room(),
                    gen_server:call(Room, {login, self()}),
                    {ok, Time2Begin} = gen_server:call(Room, {attend, self(), State#state.info}),
                    io:format("bs_player_handle_cast_attend~n"),
                    to_client(State, "{\"api\":\"attend\",\"content\":"++integer_to_list(Time2Begin)++"}\n"),
                    Room;
                true ->
                    room_closed
            end,
    {noreply, State#state{room = Room1, bet= Bet , card = Card}};
handle_cast(room_closed, State) ->
    %Alive = util:is_process_alive(State#state.tcp),
    Timeout =  300000,
    to_client(State, "{\"api\":\"room_closed\"}\n"),
    {noreply, State#state{room = room_closed},  Timeout};
handle_cast(leave_this_room, State) ->
    gen_server:call(State#state.room, {leave_room, self()}),
    %Room = bs_room_manager:find_room(),
    %gen_server:call(Room, {login, self()}),
    {noreply, State#state{ room = room_closed}};
handle_cast(game_begin, State) ->
    Dif = State#state.card*State#state.bet,
    bs_data:add_player_money(State#state.uid, - Dif),
    Info = State#state.info#userinfo{balance = list_to_binary(integer_to_list(list_to_integer(binary_to_list(State#state.info#userinfo.balance))- Dif))},
    Card1 = generate_a_new_card(),
    Card2 = generate_a_new_card(),
    to_client(State, "{\"api\":\"game_begin\",\"card1\":\""++integerlist_to_list(Card1)++
        "\",\"card2\":\""++integerlist_to_list(Card2)++"\",\"balance\":"++integer_to_list(list_to_integer(binary_to_list(State#state.info#userinfo.balance))- Dif)++"}\n"),
    {noreply, State#state{info = Info, card1 = Card1, card2 = Card2, board = []}};
handle_cast({room_wait_state, TimeRest}, State) ->
    Sign1 = "room_wait_state:",
    Signn = "\n",
    to_client(State,[ Sign1, integer_to_list(TimeRest), Signn]),
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
    to_client(State, "{\"api\":\"bingo_number\",\"content\":"++integer_to_list(Num)++"}\n"),
    {noreply, State#state{board = [ Num | State#state.board ] }};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("player unhandled msg: ~p ~n",[Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_server:cast(State#state.tcp, stop),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%non-behaviourial APIs

leave_room(Pid) ->
    gen_server:cast(Pid, leave_room).

attend(Pid, Bet, Card) ->
    gen_server:cast(Pid, {attend, Bet, Card}).

bingo(Pid, CLpairs) ->
    gen_server:cast(Pid, {bingo, CLpairs}).

bingo_info_to_client(State, #userinfo{uid=_U, balance=_B, exp=_E, name=N, avatar=A, facebookid=F} = _Info) ->
     to_client(State, "{\"api\":\"bingo_info_player\",\"name\":\""++binary_to_list(N)++
                                              "\",\"avatar\":\""++binary_to_list(A)++
                                              "\",\"facebookid\":\""++binary_to_list(F)++ 
                                              "\"}\n").

info_to_client(_State, []) ->
    ok;
info_to_client(State, [I|R]) ->
    info_to_client_(State, I),
    info_to_client(State, R).

info_to_client_(State, #userinfo{uid=_U, balance=_B, exp=_E, name=N, avatar=A, facebookid=F} = _Info) ->
    to_client(State, "{\"api\":\"player_info\",\"name\":\""++binary_to_list(N)++
                                              "\",\"avatar\":\""++binary_to_list(A)++
                                              "\",\"facebookid\":\""++binary_to_list(F)++ 
                                              "\"}\n").
%%internal functions

to_client(State, Msg) ->
    if is_pid(State#state.tcp) ->
        case util:is_process_alive(State#state.tcp) of 
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
        case util:is_process_alive(State#state.room) of 
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
