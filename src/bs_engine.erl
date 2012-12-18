-module(bs_engine).

-behaviour(gen_server).

-export([start_link/0, start_engine/0, stop_engine/0, register_listener/1,
         unregister_listener/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INTERVAL,5000).

-record(state, {listeners ,started}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    register(?MODULE,self()),
    {ok, #state{listeners = []}}.
    

handle_call({unregister, Listener}, _From, State) ->
    lists:delete(Listener, State#state.listeners),
    case State#state.started of
        true ->
            {reply, {ok, unregistered}, State , ?INTERVAL};
        _ ->
            {reply, {ok, unregistered}, State }
    end;
handle_call({register, Listener}, _From, State) ->
    NewState = #state{listeners = [ Listener | State#state.listeners ], started = State#state.started},
    case State#state.started of
        true ->
            {reply, {ok, registered}, NewState, ?INTERVAL};
        _ ->
            {reply, {ok, registered}, NewState }
    end;
handle_call(start_engine, _From, State) ->
    {reply, {ok, started}, #state{listeners = State#state.listeners, started = true}, ?INTERVAL};
handle_call(stop_engine, _From, State) ->
    {reply, {ok, stopped}, #state{listeners = State#state.listeners, started = false}, State};
handle_call(Msg, _From, State) ->
    case State#state.started of
        true ->
            {reply, {ok, Msg}, State, ?INTERVAL};
        _ ->
            {reply, {ok, Msg}, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout,State) ->
    do_engine(State),
    {noreply, State, ?INTERVAL};
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

do_engine(State) ->
    lists:foreach(fun (Listener) -> gen_server:cast(Listener,engine) end, State#state.listeners ),
    ok.