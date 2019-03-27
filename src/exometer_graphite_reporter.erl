%/--------------------------------------------------------------------
%| Copyright 2017 Erisata, UAB (Ltd.)
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%% @doc
%%% Exometer reporter module for sending metrics to a Graphite server.
%%%
-module(exometer_graphite_reporter).
-behaviour(exometer_report).
-include_lib("hut/include/hut.hrl").
-export([
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

-define(DEFAULT_GRAPHITE_URL, "localhost:2004").
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_SEND_DELAY, 10000).
-define(DEFAULT_RETRIES, 2).
-define(DEFAULT_SNAME, false).
-define(DEFAULT_PATH_PREFIX, ['$node']).
-define(DEFAULT_PERIOD_REPLACEMENT, "~").

%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(message, {
    probe       :: [atom() | integer()],
    data_point  :: atom() | integer(),
    value       :: number(),
    timestamp   :: integer()
}).

-record(state, {
    path_prefix     :: [atom() | integer() | '$node'],
    socket          :: term() | undefined,
    messages        :: [#message{}]
}).



%%% ============================================================================
%%% Callbacks for `exometer_report'.
%%% ============================================================================

%% @doc
%% Initializes the reporter and starts message sending loop.
%%
exometer_init(_Opts) ->
    SendDelay = exometer_graphite_app:get_env(send_delay, ?DEFAULT_SEND_DELAY),
    RawPathPrefix = exometer_graphite_app:get_env(path_prefix, ?DEFAULT_PATH_PREFIX),
    PeriodReplacement = exometer_graphite_app:get_env(period_replacement, ?DEFAULT_PERIOD_REPLACEMENT),
    ForceSname = exometer_graphite_app:get_env(force_sname, ?DEFAULT_SNAME),
    PathPrefix = lists:map(fun(Dir) ->
        case Dir of
            '$node' ->
                get_node(ForceSname, PeriodReplacement);
            _ -> Dir
        end
    end, RawPathPrefix),
    State = #state{
        path_prefix = PathPrefix,
        messages = []
    },
    erlang:send_after(SendDelay, self(), send),
    {ok, State}.


%% @doc
%% Receives probe, datapoint and its value from Exometer.
%%
exometer_report(Probe, DataPoint, _Extra, Value, State = #state{messages = Messages}) ->
    Message = #message{
        probe       = Probe,
        data_point  = DataPoint,
        value       = Value,
        timestamp   = erlang:system_time(seconds)
    },
    NewMessages = [Message | Messages],
    NewState = State#state{messages = NewMessages},
    {ok, NewState}.


%% @doc
%% Unused.
%%
exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_call(_Unknown, _From, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_cast(_Unknown, State) ->
    {ok, State}.


%% @doc
%% Sends collected messages to graphite and continues message sending loop.
%%
exometer_info(send, State) ->
    SendDelay = exometer_graphite_app:get_env(send_delay, ?DEFAULT_SEND_DELAY),
    erlang:send_after(SendDelay, self(), send),
    Retries = exometer_graphite_app:get_env(retries, ?DEFAULT_RETRIES),
    {ok, _NewState} = send(Retries, State);


%% @doc
%% Unused.
%%
exometer_info(_Unknown, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_newentry(_Entry, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.


%% @doc
%% Unused.
%%
exometer_terminate(_, _) ->
    ignore.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Ensures that state has a socket.
%%
ensure_connection(State = #state{socket = undefined}) ->
    HostEnv = os:getenv("EXOMETER_GRAPHITE_URL", ?DEFAULT_GRAPHITE_URL),
    [DefaultHost, DefaultPortStr] = string:tokens(HostEnv, ":"),
    Host = exometer_graphite_app:get_env(host, DefaultHost),
    Port = exometer_graphite_app:get_env(port, erlang:list_to_integer(DefaultPortStr)),
    ConnectTimeout = exometer_graphite_app:get_env(connect_timeout, ?DEFAULT_CONNECT_TIMEOUT),
    case gen_tcp:connect(Host, Port, [binary, {active, true}], ConnectTimeout) of
        {ok, NewSocket} ->
            NewState = State#state{socket = NewSocket},
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

ensure_connection(State) ->
    {ok, State}.


%%  @private
%%  Removes socket from state and makes sure it is closed.
%%
disconnect(State = #state{socket = undefined}) ->
    {ok, State};

disconnect(State) ->
    #state{
        socket = Socket
    } = State,
    try gen_tcp:close(Socket)
    catch
        _:_ -> ok
    end,
    NewState = State#state{socket = undefined},
    {ok, NewState}.


%%  @private
%%  Manages connection to a socket and sending message to Graphite.
%%
%%  All the collected messages are dropped on failure, because
%%  collecting them may cause a memory leak in the case, when
%%  graphite is inaccessible.
%%
send(0, State) ->
    ?log(error, "Error sending message. No more retries.", []),
    NewState = State#state{
        messages = []
    },
    {ok, NewState};

send(Retries, State = #state{messages = Messages}) ->
    case Messages of
        [] ->
            {ok, State};
        _ ->
            case ensure_connection(State) of
                {ok, ConnectedState} ->
                    case send(ConnectedState) of
                        {ok, AfterSentState} ->
                            {ok, AfterSentState};
                        {error, Reason} ->
                            ?log(error, "Error sending message to the graphite server, reason: ~p", [Reason]),
                            {ok, DisconnectedState} = disconnect(ConnectedState),
                            send(Retries - 1, DisconnectedState)
                    end;
                {error, Reason} ->
                    ?log(error, "Unable to connect to the graphite server, reason: ~p", [Reason]),
                    {ok, DisconnectedState} = disconnect(State),
                    send(Retries - 1, DisconnectedState)
            end
    end.


%%  @private
%%  Sends buffered messages to Graphite
%%
send(State) ->
    #state{
        path_prefix = PathPrefix,
        socket = Socket,
        messages = Messages
    } = State,
    PickleMessage = create_pickle_message(Messages, PathPrefix),
    case gen_tcp:send(Socket, PickleMessage) of
        ok ->
            NewState = State#state{messages = []},
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.


%%  @private
%%  Creates a Graphite compliant Pickle (protocol 2) message out of Exometer report.
%%  Path for Graphite metric is joined Exometer Probe and DataPoint.
%%
create_pickle_message(Messages, PathPrefix) ->
    MakePickleMessage = fun
        (#message{timestamp = undefined}) ->
            false;
        (#message{value = undefined}) ->
            false;
        (#message{probe = Probe, data_point = DataPoint, value = Value, timestamp = Timestamp}) when is_number(Value) ->
            FullProbe = lists:append(PathPrefix, Probe),
            ProbeBinary = erlang:list_to_binary(format_metric_path(FullProbe, DataPoint)),
            {true, {ProbeBinary, {Timestamp, Value}}};
        (Message) ->
            ?log(warning, "Dropping bad message: ~p", [Message]),
            false
    end,
    Payload = pickle:term_to_pickle(lists:filtermap(MakePickleMessage, Messages)),
    PayloadSize = byte_size(Payload),
    _PickleMessage = <<PayloadSize:32/unsigned-big, Payload/binary>>.



%%  @private
%%  Forms Graphite compatible metric path out of Exometer's Probe and DataPoint.
%%
format_metric_path(Probe, DataPoint) ->
    string:join(lists:map(fun metric_elem_to_list/1, Probe ++ [DataPoint]), ".").


%%  @private
%%  Converts metric path elements to list (string)
%%
metric_elem_to_list(V) when is_atom(V)    -> erlang:atom_to_list(V);
metric_elem_to_list(V) when is_binary(V)  -> erlang:binary_to_list(V);
metric_elem_to_list(V) when is_integer(V) -> erlang:integer_to_list(V);
metric_elem_to_list(V) when is_list(V)    -> V.

%%  @private
%%  Processes erlang:node() to replace periods and shorten the name
%%  to sname equivalent if force_sname is true
%%
-spec get_node(ForceSname :: boolean(), PeriodReplacement :: unicode:chardata()) -> Node :: atom().
get_node(true, PeriodReplacement) ->
    NodeName = case string:split(atom_to_list(erlang:node()), ".") of
        [Sname, _Domain] -> Sname;
        [Sname] -> Sname
    end,
    replace_period(NodeName, PeriodReplacement);
get_node(false, PeriodReplacement) ->
    replace_period(atom_to_list(erlang:node()), PeriodReplacement).

%%  @private
%%  Replaces period in a list of unicode characters with whatever
%%  is passed as PeriodReplacement and converts the list to an atom
%%
-spec replace_period(NodeName :: [unicode:chardata()], PeriodReplacement :: unicode:chardata()) -> ReplacementResult :: atom().
replace_period(NodeName, PeriodReplacement) ->
    list_to_atom(re:replace(NodeName, "\\.", PeriodReplacement, [global,{return, list}])).

%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%%  Check, if pickle message is created properly.
%%
create_pickle_message_test_() ->
    SimpleMessage = #message{
        probe       = [testA, cpuUsage],
        data_point  = value,
        value       = 0,
        timestamp   = 1499931464
    },
    [
        {"Check if payload length is calculated properly.", ?_assertEqual(
            <<0,0,0,37>>,
            binary:part(create_pickle_message([SimpleMessage], []), 0, 4)
        )},
        {"Check encoding for a single message and appending of a path prefix.", ?_assertEqual(
            <<0,0,0,51,128,2,93,40,85,34,"server1.node1.testA.cpuUsage.value",74,72,35,103,89,75,0,134,134,101,46>>,
            create_pickle_message([SimpleMessage], [server1, node1])
        )},
        {"Check if data point can be a number.", ?_assertEqual(
            <<0,0,0,46,128,2,93,40,85,29,"exometer_lager.lager.debug.50",74,72,35,103,89,75,0,134,134,101,46>>,
            create_pickle_message([#message{
                probe       = [exometer_lager,lager,debug],
                data_point  = 50,
                value       = 0,
                timestamp   = 1499931464
            }], [])
        )},
        {"Check if probe name can be numbers.", ?_assertEqual(
            <<0,0,0,28,128,2,93,40,85,11,"20.30.40.50",74,72,35,103,89,75,0,134,134,101,46>>,
            create_pickle_message([#message{
                probe       = [20,30,40],
                data_point  = 50,
                value       = 0,
                timestamp   = 1499931464
            }], [])
        )},
        {"Check if two messages are encoded properly.", ?_assertEqual(
            <<
                0,0,0,66,   % Message header
                128,2,93,   % Message info
                40,     85,20, "testA.cpuUsage.value", 74,   % Message 1
                72,35,103,89,   % Message1 timestamp
                75,
                0,              % Message1 value
                134,
                134,    85,18, "testB.memUsage.min",   74,   % Message2
                95,37,103,89,   % Message2 timestamp
                75,
                10,             % Message2 value
                134,
                134,101,46  % Message ending
            >>,
            create_pickle_message(
                [
                    #message{probe = [testA, cpuUsage], data_point = value, value = 0,  timestamp = 1499931464},
                    #message{probe = [testB, memUsage], data_point = min,   value = 10, timestamp = 1499931999}
                ], []
            )
        )}
    ].


%%
%%  Check, misc graphite path combinations.
%%
format_metric_path_test_() ->
    [
        ?_assertEqual("min",            format_metric_path([], min)),
        ?_assertEqual("dir1.dir2.max",  format_metric_path([dir1, dir2], max)),
        ?_assertEqual("dir1.dir2.max",  format_metric_path(["dir1", "dir2"], max)),
        ?_assertEqual("dir.1.max",      format_metric_path([dir, 1], max)),
        ?_assertEqual("a.first.value",  format_metric_path([a, first], value)),
        ?_assertEqual("20.first.50.75",  format_metric_path([20, first, 50], 75))
    ].



-endif.
