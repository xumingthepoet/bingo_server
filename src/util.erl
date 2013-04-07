-module(util).

-export([is_process_alive/1,kick_local_player/0]).

is_process_alive(Pid) when is_pid(Pid) ->
	rpc:call(node(Pid),erlang,is_process_alive,[Pid]);
is_process_alive(_) ->
	false.

kick_local_player() ->
	Hanlde = fun (PP) ->
				Pid = global:whereis_name(PP),
				N = node(),
				catch(
				    case node(Pid) of 
				    	N ->
				    		{ok, room_closed} = gen_server:call(Pid, 'GAMES'),
				    		gen_server:cast(Pid, stop);
				    	_ ->
				    		ok
				end
				)
			end,
	F = fun (P) -> case P of {p, P1} -> Hanlde({p, P1}); _ -> ok end end,
	lists:foreach(F,global:registered_names()).




