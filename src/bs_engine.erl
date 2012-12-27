-module(bs_engine).

-behaviour(gen_server).

-export([start_link/0, start_engine/0, stop_engine/0, register_listener/1,
         unregister_listener/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INTERVAL,1000).

-record(state, {listeners = [], started = false, lasttime}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    register(?MODULE,self()),
    {ok, #state{}}.
    

handle_call({unregister, Listener}, _From, State) ->
    Listeners = lists:delete(Listener, State#state.listeners),
    case State#state.started of
        true ->
            {reply, {ok, unregistered}, State#state{listeners = Listeners} , calculate_rest_timeout(State)};
        _ ->
            {reply, {ok, unregistered}, State#state{listeners = Listeners} }
    end;
handle_call({register, Listener}, _From, State) ->
    IsContain = lists:any(fun(E) -> E == Listener end, State#state.listeners),
    if IsContain ->
        NewState = State;
        true ->     
        NewState = State#state{listeners = [ Listener | State#state.listeners ]}
    end,
    case State#state.started of
        true ->
            {reply, {ok, registered}, NewState, calculate_rest_timeout(State)};
        _ ->
            {reply, {ok, registered}, NewState }
    end;
handle_call(start_engine, _From, State) ->
    {reply, {ok, started}, State#state{started = true}, calculate_rest_timeout(State)};
handle_call(stop_engine, _From, State) ->
    {reply, {ok, stopped}, State#state{started = false}};
handle_call(state, _From, State) ->
    case State#state.started of
        true ->
            {reply, {ok, State}, State, calculate_rest_timeout(State)};
        _ ->
            {reply, {ok, State}, State }
    end;
handle_call(Msg, _From, State) ->
    case State#state.started of
        true ->
            {reply, {ok, Msg}, State, calculate_rest_timeout(State)};
        _ ->
            {reply, {ok, Msg}, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout,State) ->
    do_engine(State),
    NewState = State#state{lasttime = now()},
    {noreply, NewState, ?INTERVAL};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_listener(Listener) ->
    gen_server:call(?MODULE, {register, Listener}).

unregister_listener(Listener) ->
    gen_server:call(?MODULE, {unregister, Listener}).

start_engine() ->
    gen_server:call(?MODULE, start_engine).

stop_engine() ->
    gen_server:call(?MODULE, stop_engine).

calculate_rest_timeout(State) ->
    case State#state.lasttime of 
        undefined ->
            ?INTERVAL;
        {A1,B1,C1} ->
            {A2,B2,C2} = now(),
            Elapse = (A2-A1)*1000000000 + (B2-B1)*1000 + ((C2-C1) div 1000),
            if (?INTERVAL > Elapse) ->
                io:format("E: ~p~n", [Elapse]),
                ?INTERVAL - Elapse;
                true ->
                0
            end
    end. 

do_engine(State) ->
    lists:foreach(fun (Listener) -> gen_server:cast(Listener,engine) end, State#state.listeners ),
    ok.