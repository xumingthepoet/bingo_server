-module(bs_data).

-include("common.hrl").

-export([get_player_data/1]).
-export([add_player_money/2]).

-define(user_table_name, <<"SlotMachine_UserTable">>).
-define(social_table_name, <<"SlotSocialUser">>).

get_player_data(UID) 
	when is_binary(UID)->
		{ UID, Exp, Bal} =  case dinerl:get_item(?user_table_name, {[{<<"HashKeyElement">>, {[{<<"S">>, UID}]}}]}, [{attrs, [<<"exp">>, <<"balance">>]}]) of 
								{ok, {Rest}} ->
									case jsonlist:get(<<"Item">>, Rest) of
 										null -> 
 											error;
                						{Item} ->	
                							{[{ <<"N">>, Exp1 }]} = jsonlist:get(<<"exp">>, Item),
                							{[{ <<"N">>, Balance1 }]} = jsonlist:get(<<"balance">>, Item),
               								{ UID, Exp1, Balance1}
                					end;
                				 _ ->
                					error
                			end,
        { Na2, Ar2, FID2}   = case dinerl:get_item(?social_table_name, {[{<<"HashKeyElement">>, {[{<<"S">>, UID}]}}]}, []) of
 									{ok, {Rest2}} ->	
 										case jsonlist:get(<<"Item">>, Rest2) of
 											null -> 
 												{<<"">>,<<"0">>,<<"0">>};
 											{Item2} ->
 												case jsonlist:get(<<"iconType">>, Item2) of
 													null ->
 														Avatar = <<"0">>;	
 													{[{ <<"S">>, Avatar }]} ->
 														ok
 												end,
 												case jsonlist:get(<<"userName">>, Item2) of
 													null ->
 														 Name = <<"">>;
 													{[{ <<"S">>, Name }]} ->
 														ok
 												end,
 												case jsonlist:get(<<"facebookId">>, Item2) of
 													null ->
 														FacebookID = <<"0">>;
 													{[{ <<"S">>, FacebookID }]} ->
 														ok
 												end,
 												{ Name, Avatar, FacebookID}
 										end;
 									_ -> 
 										{<<"">>,<<"0">>,<<"0">>}
 							    end,
 		#userinfo{uid=UID,balance=Bal,exp=Exp,name=Na2,avatar=Ar2,facebookid=FID2}.

add_player_money(UID, Score) 
	when is_binary(UID) ->
 		case dinerl:update_item(?user_table_name, {[{<<"HashKeyElement">>, {[{<<"S">>, UID}]}}]}, [{update, {[{<<"balance">>, {[{value, {[{<<"N">>, list_to_binary(integer_to_list(Score))}]}},{action, add}]}}]}}]) of
 			{ok, _R} ->
 				ok;
 			_ ->
 				error
 		end.

% get_player_data1(UID) 
% 	when is_binary(UID) ->
% 		{ UID, Exp, Bal}    = case ddb:get(?user_table_name, ddb:key_value(UID, 'string')) of
% 									{ok, Rest} ->	
% 										case jsonlist:get(<<"Item">>, Rest) of
% 											null -> 
% 												error;
% 											Item ->	
% 												[{ <<"N">>, Exp1 }] = jsonlist:get(<<"exp">>, Item),
% 												[{ <<"N">>, Balance1 }] = jsonlist:get(<<"balance">>, Item),
% 												{ UID, Exp1, Balance1}
% 										end;
% 									_ -> 
% 										error
% 	    						end,
% 		{ Na2, Ar2, FID2}   = case ddb:get(?social_table_name, ddb:key_value(UID, 'string')) of
% 									{ok, Rest2} ->	
% 										case jsonlist:get(<<"Item">>, Rest2) of
% 											null -> 
% 												{<<"">>,<<"0">>,<<"0">>};
% 											Item2 ->
% 												case jsonlist:get(<<"iconType">>, Item2) of
% 													null ->
% 														Avatar = <<"0">>;	
% 													[{ <<"S">>, Avatar }] ->
% 														ok
% 												end,
% 												case jsonlist:get(<<"userName">>, Item2) of
% 													null ->
% 														 Name = <<"">>;
% 													[{ <<"S">>, Name }] ->
% 														ok
% 												end,
% 												case jsonlist:get(<<"facebookId">>, Item2) of
% 													null ->
% 														FacebookID = <<"0">>;
% 													[{ <<"S">>, FacebookID }] ->
% 														ok
% 												end,
% 												{ Name, Avatar, FacebookID}
% 										end;
% 									_ -> 
% 										{<<"">>,<<"0">>,<<"0">>}
% 							    end,
% 		#userinfo{uid=UID,balance=Bal,exp=Exp,name=Na2,avatar=Ar2,facebookid=FID2}.


% add_player_money1(UID, Score)
% 	when is_binary(UID) ->
% 		case ddb:update(?user_table_name, ddb:key_value(UID, 'string'),
% 				[{<<"balance">>,  list_to_binary(integer_to_list(Score)) , 'number', 'add'}]) of 
% 			{ok, _R} ->
% 				ok;
% 			_ ->
% 				error
% 		end.

