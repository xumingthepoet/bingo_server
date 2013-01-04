-module(bs_data).

-export([ini/0,is_alive/1, get_alive_player_pid/1]).

ini() ->
	ok.
	
is_alive(_UID) ->
	false.

get_alive_player_pid(_UID) ->
	null.