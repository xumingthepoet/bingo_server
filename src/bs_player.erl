-module(bs_player).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([login/1]).

-record(state, {tcp,room}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).  

init([]) ->    
    {ok, #state{}, 0}.

handle_call({login,Pid}, _From, _State) ->
    Room = bs_room_manager:find_room(),
    gen_server:call(Room,{login,self()}),
    {reply, {ok, Pid}, #state{tcp = Pid, room = Room}};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({say,Content}, State) ->
    io:format("say:~p~n",[Content]),
    gen_server:cast(State#state.room,{say,Content}),
    {noreply, State};
handle_cast({room_to_player,Content}, State) ->
    io:format("reply:~p~n",[Content]),
    gen_server:cast(State#state.tcp,{to_client,Content}),
    {noreply, State};
handle_cast({new_bingo_number,Num}, State) ->
    io:format("say:~p~n",[Num]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

login(Pid) ->
	{ok,Player} = bs_player_sup:start_child(),
	{ok,_} = gen_server:call(Player,{login,Pid}),
	Player.
