-module(bs_log).

-export([init/0, info/1,info/2]).

init() ->
	ok.
	%lager:start().

info(Msg) ->
	lager:log(info, self(), Msg).

info(Text, Args) ->
	lager:log(info, self(), Text, Args).
