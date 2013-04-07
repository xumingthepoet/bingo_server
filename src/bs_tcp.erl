-module(bs_tcp). 

-behaviour(gen_server).

-include("common.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, rsock, player, is_client_active=true}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(query_player, _From, State) ->
    {reply, {ok, State#state.player}, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({to_client, Content}, State) ->
    send_data(Content, State),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Socket, RawData}, State) ->
    NewState = handle_data(RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    if 
        State#state.player /= undefine ->
            gen_server:cast(State#state.player, tcp_closed);
        true ->
            ok
    end,
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = _State) ->
    {ok, RSock} = gen_tcp:accept(LSock),
    bs_tcp_sup:start_child(),
    send_data("{\"api\":\"connected\"}", #state{rsock = RSock}),
    %bs_log:info("rsock: ~p",[RSock]),
    {noreply, #state{lsock = LSock ,rsock = RSock}};
handle_info(Msg, State) ->
    %%bs_log:info("tcp unhandled msg: ~p ~n",[Msg]),
    bs_log:info( "tcp unhandled msg: ~p ~n",[Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    (catch gen_tcp:close(State#state.rsock)),
    %bs_log:info("TERMINATE:Reason: ~p State: ~p.~n",[Reason,State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
send_data(Data, State) ->
    try
        Data_n = Data ++ "\n",
        gen_tcp:send(State#state.rsock, Data_n)
    catch
        Class:Err ->
            bs_log:info("ERROR:~p~p.~n",[Class,Err])
    end,
    State.

handle_data(<<>>, State) ->
    State#state{is_client_active = true};
handle_data(RawData, State) when is_binary(RawData) ->
    % try 
        <<Length:4/big-signed-integer-unit:8, Rest/binary>> = RawData,
        <<Msg:Length/binary, Rest2/binary>> = Rest,
        bs_log:info("Data Received : ~p ~n",[Msg]),
        {Json} = jiffy:decode(Msg),
        NewState = handle_json(Json, State),
        handle_data(Rest2, NewState).
    % catch
    %     Class:Err ->
    %         bs_log:info("ERROR:~p~p.~n",[Class,Err]);
    %     Err ->
    %         bs_log:info("ERROR:~p.~n",[Err]),
    %         State
    % end.

handle_json(Json, State) ->
    bs_log:info("Json:~p.~n",[Json]),
    case jsonlist:get(<<"api">>, Json)  of
        <<"login">> ->        
            {PlayerInfo, {ok, Player, Response}} = login( jsonlist:get(<<"uid">>, Json) ),
            Balance = PlayerInfo#userinfo.balance,
            case Response of
                room_closed ->
                    send_data("{\"api\":\"reconnect_failed\"}", State);
                no_data ->
                    send_data("{\"api\":\"login_success\",\"balance\":"++binary_to_list(Balance)++"}", State);
                {timeleft, TimeLeft} ->
                    send_data("{\"api\":\"relogin_success\",\"timeleft\":"++integer_to_list(TimeLeft)++"}", State);
                {BL, BLI, Card1, Card2, Board} ->  
                    send_data("{\"api\":\"reconnect_success\",\"card1\":\""++Card1++
                                                        "\",\"card2\":\""++Card2++
                                                        "\",\"board\":\""++Board++
                                                        "\",\"bingo_left\":"++integer_to_list(BL)++
                                                        ",\"balance\":"++binary_to_list(Balance)++"}", State);
                _ ->
                    send_data("{\"api\":\"login_fail\"}", State)
            end,
            State#state{player = Player};
        <<"attend">> ->
            Bet = jsonlist:get(<<"bet">>, Json),
            Card = jsonlist:get(<<"card">>, Json),
            bs_player:attend(State#state.player,Bet,Card),
            State;
        <<"leave_room">> ->
            bs_player:leave_room(State#state.player),
            State;
        <<"bingo">> ->
            C = jsonlist:get(<<"c">>, Json),
            L = jsonlist:get(<<"l">>, Json),
            bs_player:bingo(State#state.player, {C, L}),
            State;
        Data ->
            bs_log:info("RawData:"++Data),
            State
    end.

login(UID) ->
    login:login(UID).

login2(UID ) ->
    case bs_registration:is_logged_in(UID) of
        false ->
            create_a_new_player(UID);
        {true, Player, _PlayerState} ->
            bs_registration:login(UID, self(), Player),
            {Player, new}
    end.

create_a_new_player(UID) ->
    {ok,Player} = bs_player_sup:start_child(),
    bs_registration:login(UID, self(), Player),
    {ok,_} = gen_server:call(Player, {login, UID}),
    {Player, new}.
