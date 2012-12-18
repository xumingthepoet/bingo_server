-module(bs_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

start_link(LSock) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([LSock]) ->

	Tcp_sup = {bs_tcp_sup, {bs_tcp_sup, start_link, [LSock]},
              permanent, 2000, supervisor, [bs_tcp]},  
   
	Player_sup = {bs_player_sup, {bs_player_sup, start_link, []},
              permanent, 2000, supervisor, [bs_player]},

    Room_sup = {bs_room_sup, {bs_room_sup, start_link, []},
                permanent, 2000, supervisor, [bs_room]},

    Engine = {bs_engine, {bs_engine, start_link, []},
                permanent, 2000, worker, [bs_engine]},

    Room_manager = {bs_room_manager, {bs_room_manager, start_link, []},
                permanent, 2000, worker, [bs_room_manager]},

    Children = [Tcp_sup, Player_sup, Room_sup, Engine, Room_manager],  

    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.