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

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 2004).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_SEND_DELAY, 10000).
-define(DEFAULT_RETRIES, 2).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    host            :: string(),
    port            :: integer(),
    connect_timeout :: integer(),
    socket          :: term() | undefined,
    send_delay      :: integer(),
    retries         :: integer(),
    messages        :: list()
}).



%%% ============================================================================
%%% Callbacks for `exometer_report'.
%%% ============================================================================

%% @doc
%% Initializes the reporter and starts message sending loop.
%%
exometer_init(_Opts) ->
    Host = exometer_graphite_app:get_env(host, ?DEFAULT_HOST),
    Port = exometer_graphite_app:get_env(port, ?DEFAULT_PORT),
    ConnectTimeout = exometer_graphite_app:get_env(connect_timeout, ?DEFAULT_CONNECT_TIMEOUT),
    SendDelay = exometer_graphite_app:get_env(send_delay, ?DEFAULT_SEND_DELAY),
    Retries = exometer_graphite_app:get_env(retries, ?DEFAULT_RETRIES),
    State = #state{
        host = Host,
        port = Port,
        connect_timeout = ConnectTimeout,
        send_delay = SendDelay,
        retries = Retries,
        messages = []
    },
    erlang:send_after(SendDelay, self(), send),
    {ok, State}.


%% @doc
%% Receives probe, datapoint and its value from Exometer.
%%
exometer_report(Probe, DataPoint, _Extra, Value, State = #state{messages = Messages}) ->
    Message = create_message(Probe, DataPoint, Value),
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
    #state{
        send_delay = SendDelay,
        retries = Retries
    } = State,
    erlang:send_after(SendDelay, self(), send),
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
    #state{
        host     = Host,
        port     = Port,
        connect_timeout = ConnectTimeout
    } = State,
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
send(0, State) ->
    lager:warning("Error sending message. No more retries."),
    {ok, State};

send(Retries, State) ->
    case ensure_connection(State) of
        {ok, ConnectedState} ->
            case send(ConnectedState) of
                {ok, AfterSentState} ->
                    {ok, AfterSentState};
                {error, Reason} ->
                    lager:warning("Error sending message to the graphite server, reason: ~p", [Reason]),
                    {ok, DisconnectedState} = disconnect(ConnectedState),
                    send(Retries - 1, DisconnectedState)
            end;
        {error, Reason} ->
            lager:warning("Unable to connect to the graphite server, reason: ~p", [Reason]),
            {ok, DisconnectedState} = disconnect(State),
            send(Retries - 1, DisconnectedState)
    end.


%%  @private
%%  Sends buffered messages to Graphite
%%
send(State = #state{socket = Socket, messages = Messages}) ->
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


%%  @private
%%  Passes current unix timestamp to create_message/4.
%%
create_message(Probe, DataPoint, Value) ->
    create_message(Probe, DataPoint, Value, erlang:system_time(seconds)).


%%  @private
%%  Creates a Graphite compliant Pickle (protocol 2) message out of Exometer report.
%%  Path for Graphite metric is joined Exometer Probe and DataPoint.
%%
create_message(Probe, DataPoint, Value, Timestamp) ->
    ProbeBinary = erlang:list_to_binary(format_metric_path(Probe, DataPoint)),
    Payload = pickle:term_to_pickle([{ProbeBinary, {Timestamp, Value}}]),
    PayloadSize = byte_size(Payload),
    Message = <<PayloadSize:32/unsigned-big, Payload/binary>>,
    Message.


%%  @private
%%  Forms Graphite compatible metric path out of Exometer's Probe and DataPoint.
%%
format_metric_path(Probe, DataPoint) ->
    string:join(lists:map(fun metric_elem_to_list/1, Probe ++ [DataPoint]), ".").


%%  @private
%%  Converts metric path elements to list (string)
%%
metric_elem_to_list(V) when is_atom(V) -> erlang:atom_to_list(V);
metric_elem_to_list(V) when is_integer(V) -> erlang:integer_to_list(V);
metric_elem_to_list(V) when is_list(V) -> V.



%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%%  Check, if pickle message is created properly.
%%
create_message_test_() ->
    [
        ?_assertEqual(<<0,0,0,37,128,2,93,40,85,20,"testZ.cpuUsage.value",
            74,72,35,103,89,75,0,134,134,101,46>>,
            create_message([testZ, cpuUsage], value, 0, 1499931464)),
        ?_assertEqual(<<0,0,0,37>>,
            binary:part(create_message([testZ, cpuUsage], value, 0, 1499931464),
                0, 4)),
        ?_assertEqual(<<0,0,0,46,128,2,93,40,85,29,"exometer_lager.lager.debug.50",
            74,72,35,103,89,75,0,134,134,101,46>>,
            create_message([exometer_lager,lager,debug], 50, 0, 1499931464)),
        ?_assertEqual(<<0,0,0,28,128,2,93,40,85,11,"20.30.40.50",
            74,72,35,103,89,75,0,134,134,101,46>>,
            create_message([20,30,40], 50, 0, 1499931464))
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
