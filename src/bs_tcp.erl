-module(bs_tcp). 

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, rsock,player}).

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
    NewState = handle_data( RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = _State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    bs_tcp_sup:start_child(),
    io:format("rsock: ~p",[_Sock]),
    {noreply, #state{lsock = LSock ,rsock = _Sock}}.

terminate(Reason, State) ->
    io:format("TERMINATE:Reason: ~p State: ~p.~n",[Reason,State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
send_data(RawData, State) ->
    try
        Socket = State#state.rsock,
        io:format("ERROR:~p~p.~n",[Socket,RawData]),
        gen_tcp:send(Socket,RawData)
    catch
        Class:Err ->
            io:format("ERROR:~p~p.~n",[Class,Err])
    end,
    State.

handle_data(RawData, State) ->
    try
        case bs_protocol:parse(RawData) of 
            {"login",[_|Content]} -> 
                io:format("RawData:."++Content),
                Player = bs_player:login(self()),
                io:format("Player:~p.",[Player]),
                io:format("State:~p.",[State]),
                NewState = #state{ lsock=State#state.lsock,rsock=State#state.rsock, player=Player },
                NewState;
            {"say",[_|Content]} ->
                gen_server:cast(State#state.player,{say,Content}),
                State;
            Data ->
                io:format("RawData:."++Data),
                State
            end
    catch
        Class:Err ->
            io:format("ERROR:~p ~p.~n", [Class,Err]),
            State
    end.

