-module(analysis).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([begin_game/1, bingo/1, result/0]).

-record(state, {today_bet=1, today_win=0, yesterday_bet=1, yesterday_win=0, date}).

start_link() ->
    gen_server:start_link(?MODULE, [], []). 

init([]) ->
	register(?MODULE,self()),
    {ok, #state{date=date()}}. 

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.


handle_cast({bet, Bet}, State) ->
	State1 = check_date(State),
	State2 = State1#state{today_bet=(State1#state.today_bet+Bet)},
    {noreply, State2};
handle_cast({win, Win}, State) ->
	State1 = check_date(State),
	State2 = State1#state{today_win=(State1#state.today_win+Win)},
    {noreply, State2};
handle_cast(result, State) ->
	bs_log:info("date ~p : today_bet=~p, today_win=~p, yesterday_bet=~p, yesterday_win=~p",
	    [State#state.date, State#state.today_bet, State#state.today_win, State#state.yesterday_bet, State#state.yesterday_win]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State }.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

begin_game(Bet) ->
	gen_server:cast(?MODULE, {bet, Bet}).

bingo(Win) ->
	gen_server:cast(?MODULE, {win, Win}).

result() ->
	gen_server:cast(?MODULE, result).

%% internal functions

check_date(State) ->
	Date = State#state.date,
	case date() of
		Date  ->
			State;
		_ ->
			State#state{yesterday_win=State#state.today_win, yesterday_bet=State#state.today_bet, today_bet=1, today_win=0,  date=date()}
	end.

