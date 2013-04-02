-module(bs_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1234).

-define(TCP_OPTIONS, [binary, {active, true}, {packet, 0}, {reuseaddr, true}]).

start(_StartType, _StartArgs) ->
    io:format("bs_app start ... ~n"),
    ok = initDynamoDB(),
    io:format("initDynamoDB initialized already. ~n"),
    Port = case application:get_env(tcp_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("bs_sup start ... ~n"),
    case bs_sup:start_link(LSock) of
        {ok, Pid} ->
            bs_sup:start_child(),
            % bs_engine:start_engine(),
            bs_tcp_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

initDynamoDB() ->
    inets:start(),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lhttpc),
    dinerl:setup("AKIAISPHG46EFLNNHUJQ", "Hd2h3ZUxP/MP51WElYWeDsLmcfayKOFdaXEpANgN", "us-east-1a"),
    ok.
% initDynamoDB() ->
%     inets:start(),
%     ssl:start(),
%     application:start(ibrowse),
%     ddb_iam:credentials("AKIAISPHG46EFLNNHUJQ", "Hd2h3ZUxP/MP51WElYWeDsLmcfayKOFdaXEpANgN"),
%     {'ok', Key, Secret, Token} = ddb_iam:token(129600),
%     ddb:credentials(Key, Secret, Token),
%     ok.