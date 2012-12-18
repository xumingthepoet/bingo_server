-module(bs_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1234).

-define(TCP_OPTIONS, [binary, {active, true}, {packet, 0}, {reuseaddr, true}]).

start(_StartType, _StartArgs) ->
    Port = case application:get_env(tcp_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    case bs_sup:start_link(LSock) of
        {ok, Pid} ->
            bs_sup:start_child(),
            bs_engine:start_engine(),
            bs_tcp_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.