-module(jsonlist).

-export([get/2]).

get(_Key, []) ->
	null;
get(Key, [Pre| Rest]) ->
	{K,	Content} = Pre,
	case K of
		Key ->
			Content;
		_ ->
			get(Key, Rest)
	end.