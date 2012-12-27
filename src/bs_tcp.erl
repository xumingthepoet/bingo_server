-module(bs_tcp). 

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, rsock, player}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({to_client, Content}, State)->
    send_data(Content, State),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Socket, RawData}, State) ->
    Data = binary_to_list(RawData),
    NewState = handle_data(Data, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = _State) ->
    {ok, RSock} = gen_tcp:accept(LSock),
    send_data("connect success\n", #state{rsock = RSock}),
    bs_tcp_sup:start_child(),
    io:format("rsock: ~p",[RSock]),
    {noreply, #state{lsock = LSock ,rsock = RSock}}.

terminate(Reason, State) ->
    io:format("TERMINATE:Reason: ~p State: ~p.~n",[Reason,State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
send_data(RawData, State) ->
    try
        Socket = State#state.rsock,
        gen_tcp:send(Socket,RawData)
    catch
        Class:Err ->
            io:format("ERROR:~p~p.~n",[Class,Err])
    end,
    State.

handle_data([], State) ->
    State;
handle_data(RawData, State) ->
    try
        {Command, Content, Rest} = parse(RawData),
        case Command of 
            "{login" ->        
                Player = bs_player:login( Content),
                send_data("login success\n", State),
                handle_data(Rest, State#state{player=Player});
            "{attend" ->
                bs_player:attend(State#state.player),
                handle_data(Rest, State);
            Data ->
                io:format("RawData:"++Data),
                State
            end
    catch
        Class:Err ->
            io:format("ERROR:~p ~p ~p.~n", [Class, Err, RawData]),
            State
    end.

parse(Data) ->
    {Command,[_|Contents]} = lists:splitwith(fun(T) -> [T] =/= ":" end , Data),
    {Content, [_|Rest]} = lists:splitwith(fun(T) -> [T] =/= "}" end , Contents),
    {Command, Content, Rest}.



