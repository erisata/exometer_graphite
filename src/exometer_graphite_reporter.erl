-module(exometer_graphite_reporter).
-behaviour(exometer_reporter).

%% gen_server callbacks
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-include_lib("exometer_core/include/exometer.hrl").

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 2004).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).

-record(st, {
          host = ?DEFAULT_HOST,
          port = ?DEFAULT_PORT,
          connect_timeout = ?DEFAULT_CONNECT_TIMEOUT,
          name,
          namespace = [],
          prefix = [],
          api_key = "",
          socket = undefined}).

-include("log.hrl").

% funkcija, kuri issiuncia hardcoded testine metrika 
send_test_metric() ->
    Payload = pickle:term_to_pickle([{<<"julyXtest.cpuUsage">>, {erlang:system_time(seconds), 1}}]),
    N = byte_size(Payload),
    Message = <<N:32/unsigned-big, Payload/binary>>,

    {ok, Socket} = gen_udp:open(8000),
    ok = gen_udp:send(Socket, {127, 0, 0, 1}, 2004, Message).

exometer_init(Opts) ->
    io:format("exometer_init CALLED~n"),
    Host = ?DEFAULT_HOST,
    Port = ?DEFAULT_PORT,
    ConnectTimeout = ?DEFAULT_CONNECT_TIMEOUT,

    case gen_tcp:connect(Host, Port, [binary, {active, true}], ConnectTimeout) of
        {ok, Sock} ->
            io:format("connected successfully~n"),
            {ok, #st{socket = Sock,
                    host = Host,
                    port = Port,
                    connect_timeout = ConnectTimeout }};
        {error, _} = Error ->
            Error
    end.

exometer_report(Probe, DataPoint, _Extra, Value, #st{socket = Sock,
                                                    api_key = APIKey,
                                                    prefix = Prefix} = St) ->
    io:format("exometer_report CALLED. Probe: ~p, DataPoint ~p, Value ~p~n", [Probe, DataPoint, Value]),
    
    Payload = pickle:term_to_pickle([{<<"july99test.cpuUsage">>, {erlang:system_time(seconds), random:uniform(5)}}]),
    N = byte_size(Payload),
    Message = <<N:32/unsigned-big, Payload/binary>>,

    case gen_tcp:send(Sock, Message) of
        ok ->
            {ok, St};
        _ ->
            reconnect(St)
    end.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St }.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St }.

exometer_call(Unknown, From, St) ->
    io:format("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    io:format("Unknown cast: ~p", [Unknown]),
    {ok, St}.

exometer_info(Unknown, St) ->
    io:format("Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  -> error({required, K})
    end.

reconnect(St) ->
    case gen_tcp:connect(St#st.host, St#st.port, [binary, {active, true}], St#st.connect_timeout) of
        {ok, Sock} ->
            io:format("reconnected successfully~n"),
            {ok, #st{socket = Sock}};
        {error, _} = Error ->
            Error
    end.

get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).
