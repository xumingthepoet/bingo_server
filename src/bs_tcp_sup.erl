-module(bs_tcp_sup).

-behaviour(supervisor).

-export([start_link/1,
         start_child/0
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([LSock]) ->
    Tcp = {bs_tcp, {bs_tcp, start_link, [LSock]},
               temporary, brutal_kill, worker, [bs_tcp]},
    Children = [Tcp],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.