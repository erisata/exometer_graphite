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
%%% Module for Exometer and Graphite integration.
%%% Modify sys.config to change integration settings.
%%% 
-module(exometer_graphite_reporter).
-behaviour(exometer_reporter).
-compile([{parse_transform, lager_transform}]).
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
-include_lib("exometer_core/include/exometer.hrl").

-define(APP, exometer_graphite).
-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 2004).
-define(DEFAULT_CONNECTION_TIMEOUT, 5000).
-define(DEFAULT_GRAPHITE_DELAY, 10000).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    host                :: string(),
    port                :: integer(),
    connection_timeout  :: integer(),
    socket              :: term() | undefined,
    messages            :: list(),
    graphite_delay      :: integer()
}).



%%% ============================================================================
%%% Callbacks for `exometer_report'.
%%% ============================================================================

%% @doc
%% Initializes the reporter and starts message sending loop.
%%
exometer_init(_Opts) ->
    Host = application:get_env(?APP, host, ?DEFAULT_HOST),
    Port = application:get_env(?APP, port, ?DEFAULT_PORT),
    ConnectionTimeout = application:get_env(?APP, connection_timeout, ?DEFAULT_CONNECTION_TIMEOUT),
    GraphiteDelay = application:get_env(?APP, graphite_delay, ?DEFAULT_GRAPHITE_DELAY),
    State = #state{
                host = Host,
                port = Port,
                connection_timeout = ConnectionTimeout,
                graphite_delay = GraphiteDelay,
                messages = []},
    erlang:send_after(GraphiteDelay, self(), send),
    {ok, State}.

    
%% @doc
%% Receives probe, datapoint and its value from Exometer.
%%
exometer_report(Probe, DataPoint, _Extra, Value, State) ->
    #state{
        messages = Messages
    } = State,
    lager:info("exometer_report CALLED. Probe: ~p, DataPoint ~p, Value ~p", 
        [Probe, DataPoint, Value]),
    Message = create_message(Probe, DataPoint, Value),
    NewMessages = [Message | Messages],
    NewState = State#state{messages = NewMessages},
    {ok, NewState}.


%% @doc
%% Unused
%%
exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.


%% @doc
%% Unused
%%
exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.


%% @doc
%% Unused
%%
exometer_call(Unknown, From, State) ->
    lager:info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, State}.


%% @doc
%% Unused
%%
exometer_cast(Unknown, State) ->
    lager:info("Unknown cast: ~p", [Unknown]),
    {ok, State}.


%% @doc
%% Sends collected messages to graphite and continues message sending loop.
%%
exometer_info(send, State) ->
    #state{
        graphite_delay = GraphiteDelay
    } = State,
    erlang:send_after(GraphiteDelay, self(), send),
    {ok, _NewState} = send(2, State);


%% @doc
%% Unused
%%
exometer_info(Unknown, State) ->
    lager:info("Unknown info: ~p", [Unknown]),
    {ok, State}.


%% @doc
%% Unused
%%
exometer_newentry(_Entry, State) ->
    {ok, State}.


%% @doc
%% Unused
%%
exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.


%% @doc
%% Unused
%%
exometer_terminate(_, _) ->
    ignore.    



%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%% @doc
%% Ensures that state has a socket.
%%
ensure_connection(State = #state{socket = undefined}) ->
    #state{
        host     = Host,
        port     = Port,
        connection_timeout = ConnectionTimeout
    } = State,
    case gen_tcp:connect(Host, Port, [binary, {active, true}], ConnectionTimeout) of
        {ok, NewSocket} ->
            NewState = State#state{socket = NewSocket},
            lager:info("returning NewSocket: ~p", [NewSocket]),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

ensure_connection(State) ->
    {ok, State}.


%% @doc
%% Removes socket from state and makes sure it is closed.
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


%% @doc
%% Manages connection to a socket and sending message to Graphite.
%%
send(0, State) ->
    lager:error("Error sending message. No more retries."),
    {ok, State};

send(Retries, State) ->
    case ensure_connection(State) of
        {ok, ConnectedState} ->
            case send(ConnectedState) of
                {ok, AfterSentState} ->
                    {ok, AfterSentState};
                {error, Reason} ->
                    lager:error("Error sending message. Reason: ~p", [Reason]),
                    {ok, DisconnectedState} = disconnect(ConnectedState),
                    send(Retries - 1, DisconnectedState)
            end;
        {error, Reason} ->
            lager:error("Unable to connect... Reason: ~p", [Reason]),
            {ok, DisconnectedState} = disconnect(State),
            send(Retries - 1, DisconnectedState)
    end.


%% @doc
%% Sends buffered messages to Graphite
%%
send(State) ->
    #state{
        socket = Socket,
        messages = Messages
    } = State,
    lager:info("Messages: ~p", [Messages]),
    case Messages of
        [] ->
            {ok, State};
        _ ->
            case gen_tcp:send(Socket, lists:reverse(Messages)) of
                ok ->
                    NewState = State#state{messages = []},
                    {ok, NewState};
                {error, Reason} ->
                    {error, Reason}
            end
    end.


%%
%% Passes current unix timestamp to create_message/4.
%%
create_message(Probe, DataPoint, Value) ->
    create_message(Probe, DataPoint, Value, erlang:system_time(seconds)).


%%
%% Creates a Graphite compliant Pickle (protocol 2) message out of Exometer report.
%% Path for Graphite metric is joined Exometer Probe and DataPoint.
%%
create_message(Probe, DataPoint, Value, Timestamp) ->
    ProbeBinary = erlang:list_to_binary(format_metric_path(Probe, DataPoint)),
    Payload = pickle:term_to_pickle([{ProbeBinary, {Timestamp, Value}}]),
    N = byte_size(Payload),
    Message = <<N:32/unsigned-big, Payload/binary>>,
    Message.


%%
%% Forms Graphite compatible metric path out of Exometer's Probe and DataPoint.
%%
format_metric_path(Probe, DataPoint) ->   % test
    string:join(lists:map(fun erlang:atom_to_list/1, Probe ++ [DataPoint]), ".").



%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_message_test_() ->
    [
        ?_assertEqual(<<0,0,0,37,128,2,93,40,85,20,"testZ.cpuUsage.value",
            74,72,35,103,89,75,0,134,134,101,46>>,
            create_message([testZ, cpuUsage], value, 0, 1499931464)),
        ?_assertEqual(<<0,0,0,37>>,
            binary:part(create_message([testZ, cpuUsage], value, 0, 1499931464),
                0, 4))
    ].


format_metric_path_test_() ->
    [
        ?_assertEqual("min", format_metric_path([], min)),
        ?_assertEqual("dir1.dir2.max", format_metric_path([dir1, dir2], max)),
        ?_assertError(badarg, format_metric_path(["dir1", "dir2"], max)),
        ?_assertEqual("dir.1.max", format_metric_path([dir, 1], max)),
        ?_assertEqual("a.first.value", format_metric_path([a, first], value))
        ].

-endif.