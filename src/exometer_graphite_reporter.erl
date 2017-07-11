-module(exometer_graphite_reporter).
-compile([{parse_transform, lager_transform}]).
-behaviour(exometer_reporter).

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
-define(SEND_TO_GRAPHITE_INTERVAL, 10000).

-record(state, {
          host = ?DEFAULT_HOST,
          port = ?DEFAULT_PORT,
          connect_timeout = ?DEFAULT_CONNECT_TIMEOUT,
          name,
          namespace = [],
          prefix = [],
          api_key = "",
          socket = undefined,
          messages = []}).

-include("log.hrl").

exometer_init(Opts) ->
    lager:info("exometer_init CALLED"),
    Host = ?DEFAULT_HOST,
    Port = ?DEFAULT_PORT,
    ConnectTimeout = ?DEFAULT_CONNECT_TIMEOUT,

    case gen_tcp:connect(Host, Port, [binary, {active, true}], ConnectTimeout) of
        {ok, Sock} ->
            lager:info("connected successfully"),
            erlang:send_after(?SEND_TO_GRAPHITE_INTERVAL, self(), send),
            {ok, #state{socket = Sock,
                    host = Host,
                    port = Port,
                    connect_timeout = ConnectTimeout }};
        {error, _} = Error ->
            Error
    end.
    

exometer_report(Probe, DataPoint, _Extra, Value, #state{socket = Sock,
                                                    api_key = APIKey,
                                                    prefix = Prefix,
                                                    messages = Messages} = State) ->
    lager:info("exometer_report CALLED. Probe: ~p, DataPoint ~p, Value ~p", [Probe, DataPoint, Value]),
    
    ProbeBinary = list_to_binary(format_probe(Probe, DataPoint)),

    Payload = pickle:term_to_pickle([{ProbeBinary, {erlang:system_time(seconds), Value}}]),
    N = byte_size(Payload),
    Message = <<N:32/unsigned-big, Payload/binary>>,
    
    NewMessages = [Message|Messages],
    {ok, State#state{messages = NewMessages}}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_call(Unknown, From, State) ->
    lager:info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, State}.

exometer_cast(Unknown, State) ->
    lager:info("Unknown cast: ~p", [Unknown]),
    {ok, State}.

exometer_info(send, State = #state{socket = Socket, messages = Messages}) ->    
    lager:info("exometer_info invoked"),
    lager:info("Messages: ~p", [Messages]),
    case length(Messages) of
        0 ->
            erlang:send_after(?SEND_TO_GRAPHITE_INTERVAL, self(), send),
            {ok, State};
        _ ->
            lager:info("sending aggregated message"),
            case gen_tcp:send(Socket, Messages) of
                ok ->
                    lager:info("sent successfully!"),
                    erlang:send_after(?SEND_TO_GRAPHITE_INTERVAL, self(), send),
                    {ok, State#state{messages = []}};
                _ ->
                    reconnect(State)
            end            
    end;
    

exometer_info(Unknown, State) ->
    lager:info("Unknown info: ~p", [Unknown]),
    {ok, State}.

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_, _) ->
    ignore.
    
format_probe(Probe, DataPoint) ->
    string:join(lists:map(fun metric_elem_to_list/1, Probe), ".") ++ "." ++ atom_to_list(DataPoint).

metric_elem_to_list(V) when is_atom(V) -> atom_to_list(V);
metric_elem_to_list(V) when is_binary(V) -> binary_to_list(V);
metric_elem_to_list(V) when is_integer(V) -> integer_to_list(V);
metric_elem_to_list(V) when is_list(V) -> V.

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  -> error({required, K})
    end.

reconnect(State = #state{host = Host, port = Port, connect_timeout = ConnectTimeout}) ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}], ConnectTimeout) of
        {ok, Sock} ->
            lager:info("reconnected successfully"),
            {ok, #state{socket = Sock}};
        {error, _} = Error ->
            Error
    end.

get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).
