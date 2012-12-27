-module(bs_data).

-export([is_alive/1, get_alive_player_pid/1]).

is_alive(_UID) ->
	false.

get_alive_player_pid(_UID) ->
	null.