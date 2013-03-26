-module(bs_registration).

-export([ini/0,is_logged_in/1, login/3]).

-define(bingo_table_name, <<"SlotMachine_Bingo">>).

ini() ->
	ok.
	
is_logged_in(UID) ->	
	case ddb:get(?bingo_table_name, ddb:key_value(UID, 'string')) of
		{ok, Rest} ->	
				case jsonlist:get(<<"Item">>, Rest) of
					null -> 
						false;
					Item ->	
						[{ <<"S">>, Tcp }] = jsonlist:get(<<"tcp">>, Item),
						[{ <<"S">>, Player }] =  jsonlist:get(<<"player">>, Item),
						is_player_active(UID, list_to_pid(binary_to_list(Tcp)), list_to_pid(binary_to_list(Player)))
				end;
		_ -> 
			false
	end.

login(UID, Tcp, Player) ->
	ddb:update(?bingo_table_name, ddb:key_value(UID, 'string'),
                    [{<<"tcp">>, list_to_binary(pid_to_list(Tcp)), 'string', 'put'},
                    {<<"player">>, list_to_binary(pid_to_list(Player)), 'string', 'put'}]),
	ok.
	
is_player_active(UID, Tcp, Player) ->
	%io:format("tcp: ~p , player : ~p ", [Tcp, Player]),
	case util:is_process_alive(Player) of 
		true ->	
			{ok , Room, UID2} = 
				try 
					gen_server:call(Player,query_player_state)
				catch
					_ ->
						{ok, null_state, null_uid}
				end,
			if 
				UID == UID2 ->
					catch(gen_server:call(Player,change_tcp)),
					case util:is_process_alive(Tcp) of
						true ->	
							{ok, Player2} = 
								try
									gen_server:call(Tcp,query_player)
								catch
									_ -> 
										{ok, null_player}
								end,
							if
								Player == Player2 ->
									gen_server:cast(Tcp,stop);
								true ->
									ok
							end;
						_ ->
							ok
					end,
					{true, Player, Room};
				true ->
					fasle
			end;
		_ ->
			false
	end.

