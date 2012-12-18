-module(bs_player_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/0
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    Player = {bs_player, {bs_player, start_link, []},
                temporary, brutal_kill, worker, [bs_player]},
    Children = [Player],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.