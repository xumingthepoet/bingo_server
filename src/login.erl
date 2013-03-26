-module(login).

-export([login/1]).

-include("common.hrl").

login(UID) 
	when is_binary(UID) ->
		PlayInfo = bs_data:get_player_data(UID),
		login(PlayInfo, UID).

login(PlayerInfo, UID) ->
	Socket = global:whereis_name({?SOCKET,UID}),
	Process2 = global:whereis_name({?PROCESS,UID}),
    gen_server:cast(Process2, dont_die),
    timer:sleep(100),
    Process = global:whereis_name({?PROCESS,UID}),

	Condition = check_player(PlayerInfo, Socket, Process, 
                            [
                                fun is_player_busy/3,
                                fun is_player_online/3,
                                fun is_client_down/3,
                                fun is_offline/3
                            ]),
    global:unregister_name({?SOCKET,UID}),
    stop_socket(Socket),
	Result = login(PlayerInfo, Process, Condition),
	global:register_name({?SOCKET,UID}, self()),
	{PlayerInfo, Result}.

login(Info, Process, player_online) ->
    %% player is idle
    %% gen_server:call(Process, 'LOG_OUT'),
    login(Info, Process, client_down);
login(Info, Process, player_busy) ->
    login(Info, Process, client_down);
login(_Info, Process, client_down) ->
    %% tell player process to talk to the new socket
    {ok, Response} = gen_server:call(Process, 'RESET_SOCKET'),
    {ok, Process, Response};    
login(Info, _Process, player_offline) ->
    %% start player process
    {ok, Pid} = bs_player_sup:start_child(Info),
    gen_server:call(Pid, 'REGISTER_SOCKET'),
    {ok, Pid, no_data}.

check_player(PlayerInfo, Socket, Process, [Fun|Rest]) ->
	case Fun(PlayerInfo, Socket, Process) of
		{true, Condition} ->
			Condition;
		_ ->
			check_player(PlayerInfo, Socket, Process, Rest)
	end;
check_player(_,_,_,[]) ->
	unknown_login_condition_error.

is_player_busy(PlayerInfo, Socket, Process) ->
    {Online, _} = is_player_online(PlayerInfo, Socket, Process),
    {ok, Games} =   if
                        Process /= undefined ->
                            gen_server:call(Process, 'GAMES');
                        true ->
                            {ok, room_closed}
                    end,
    Playing = Games /= room_closed,
    {Online and Playing, player_busy}.

is_player_online(_, Socket, Process) ->
    SocketAlive = Socket /= undefined,
    PlayerAlive = Process /= undefined,
    {SocketAlive and PlayerAlive, player_online}.

is_client_down(_, Socket, Process) ->
    SocketDown = Socket == undefined,
    PlayerAlive = Process /= undefined,
    {SocketDown and PlayerAlive, client_down}.

is_offline(_, _Socket, Process) ->
    % SocketDown = Socket == undefined,
    PlayerDown = Process == undefined,
    {PlayerDown, player_offline}.

stop_socket(Socket) ->
    if
        Socket /= undefined ->
            gen_server:cast(Socket, stop);
        true ->
            do_nothing
    end.



