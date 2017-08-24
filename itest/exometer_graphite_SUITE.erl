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

%%%
%%% Common Tests for `exometer_graphite_reporter` module.
%%%
-module(exometer_graphite_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).
-export([
    test_message_sending/1,
    test_static_configuration/1
]).

-define(APP, exometer_graphite).
-define(DEFAULT_TCP_SERVER_MOCK_PORT, 8049).
-define(REPORTER, exometer_graphite_reporter).

%%% ============================================================================
%%% Callbacks for `common_test`
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_message_sending,
    test_static_configuration
].


%%
%%  Test case test_message_sending initialization.
%%
init_per_testcase(test_message_sending, Config) ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    application:set_env(?APP, subscriptions, []),
    {ok, Apps} = application:ensure_all_started(?APP),
    [{exometer_graphite_apps, Apps} | Config];

init_per_testcase(test_static_configuration, Config) ->
    application:load(?APP),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(?APP),
    [{exometer_graphite_apps, Apps} | Config].


%%
%%  Test case test_message_sending ending.
%%
end_per_testcase(test_message_sending, _Config) ->
    lists:foreach(fun({Metric, _, _, _}) ->
        exometer_report:unsubscribe_all(?REPORTER, Metric)
                  end, exometer_report:list_subscriptions(?REPORTER)),
    ok;

end_per_testcase(test_static_configuration, _Config) ->
    lists:foreach(fun({Metric, _, _, _}) ->
        exometer_report:unsubscribe_all(?REPORTER, Metric)
                  end, exometer_report:list_subscriptions(?REPORTER)),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%  Tests if grouped pickle message sending to mock server is
%%  successful. Dynamic metric and subscription configuration.
%%
test_message_sending(_Config) ->
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    tcp_server_mock:start(Port, self()),
    exometer:new([testB, memUsage], histogram),
    exometer:new([testZ, cpuUsage], gauge),
    exometer_report:subscribe(?REPORTER, [testB, memUsage], min, 2000, []),
    exometer_report:subscribe(?REPORTER, [testZ, cpuUsage], value, 2500, []),
    exometer:update([testB, memUsage], 10),
    Message = receive
        {received, Data} ->
            lager:debug("Successfully received message."),
            Data;
        _Other ->
            lager:debug("Unexpected message.")
    after
        4000 ->
            lager:debug("Did NOT receive message"),
            ok
    end,
    <<Length:(4*8), Info:(6*8), Path:(18*8), Separator:(1*8),
        _Timestamp:(4*8), Ending:(6*8), _OtherMsgs/binary>> = Message,
    TimestampNow = binary:encode_unsigned(erlang:system_time(seconds), little),
    ExpectedMessage = <<
        0,0,0,66,   % Message header
        128,2,93,   % Message start
        40,     85,20, "testZ.cpuUsage.value", 74,72,35,103,89,75,0, 134,   % Message 1
        134,    85,18, "testB.memUsage.min",   74,95,37,103,89,75,10,134,   % Message 2
        134,101,46  % Ending
    >>,
%
%
%     <<0,0,0,66,128,2,93,40,85,18,"testB.memUsage.min",74,TimestampNow/binary,75,10,134,
%         134,85,20,"testZ.cpuUsage.value",74,TimestampNow/binary,75,0,134,134,101,46>>,
    <<
        ExpectedLength:(4*8),
        ExpectedInfo:(3*8),
        _Msg1Start:24, ExpectedPath1:(18*8),  ExpectedSeparator1:(1*8), _ExpectedTimestamp1:(4*8),  ExpectedEnding1:(3*8),
        _Msg2Start:24, _ExpectedPath2:(20*8), _ExpectedSeparator2:(1*8), _ExpectedTimestamp2:(4*8), _ExpectedEnding2:(3*8),
        _MsgEnding:24
    >> = ExpectedMessage,
    ExpectedLength = Length,
    ExpectedInfo   = Info,
    ExpectedPath1 = Path,
    ExpectedSeparator1 = Separator,
    % Timestamp check ignored
    ExpectedEnding1 = Ending.


%%  @doc
%%  Check, if static subscriptions are passed to exometer report.
%%  Check, if a future subscription is working when its metric is added.
%%  Check, if subscription is removed if a metric is removed.
%%
test_static_configuration(_Config) ->
    exometer:new([eproc_core, lager, warning], histogram),
    exometer:new([eproc_core, store, get_message], spiral),
    %
    ok = exometer:update_or_create([testEL, lager, debug        ], 1, histogram, []),
    ok = exometer:update_or_create([exometer_lager, lager, info ], 1, histogram, []),
    %
    % Forcing resubscribe, though it would be automatic resubscription by interval.
    ok = exometer_graphite_subscribers:force_resubscribe(),
    timer:sleep(200),
    % sort, ActualSubs remove
    ExpectedSubs = [
        {[testEL,lager,debug],min,4000,[]},
        {[testEL,lager,debug],mean,4000,[]},
        {[testEL,lager,debug],max,4000,[]},
        {[exometer_lager,lager,info],n,5000,[]},
        {[exometer_lager,lager,info],min,5000,[]},
        {[exometer_lager,lager,info],median,5000,[]},
        {[exometer_lager,lager,info],mean,5000,[]},
        {[exometer_lager,lager,info],max,5000,[]},
        {[exometer_lager,lager,info],999,5000,[]},
        {[exometer_lager,lager,info],99,5000,[]},
        {[exometer_lager,lager,info],95,5000,[]},
        {[exometer_lager,lager,info],90,5000,[]},
        {[exometer_lager,lager,info],75,5000,[]},
        {[exometer_lager,lager,info],50,5000,[]},
        {[eproc_core,store,get_message],one,10000,[]},
        {[eproc_core,store,get_message],count,10000,[]},
        {[eproc_core,lager,warning],min,20000,[]},
        {[eproc_core,lager,warning],mean,20000,[]},
        {[eproc_core,lager,warning],max,20000,[]}],
    ExpectedSubs = exometer_report:list_subscriptions(exometer_graphite_reporter),
    %
    % Checking a situation when a new metric fitting subscription is added.
    exometer:new([eproc_core, store, attachment_delete], spiral),
    exometer:delete([eproc_core, lager, warning]),
    ok = exometer_graphite_subscribers:force_resubscribe(),
    timer:sleep(200),
    %
    % If same subscription is added to exometer twice, only one will be shown in
    % exometer_report:list_subscriptions, though actually there would be two.
    NewerExpectedSubs = [
        {[testEL,lager,debug],min,4000,[]},
        {[testEL,lager,debug],mean,4000,[]},
        {[testEL,lager,debug],max,4000,[]},
        {[exometer_lager,lager,info],n,5000,[]},
        {[exometer_lager,lager,info],min,5000,[]},
        {[exometer_lager,lager,info],median,5000,[]},
        {[exometer_lager,lager,info],mean,5000,[]},
        {[exometer_lager,lager,info],max,5000,[]},
        {[exometer_lager,lager,info],999,5000,[]},
        {[exometer_lager,lager,info],99,5000,[]},
        {[exometer_lager,lager,info],95,5000,[]},
        {[exometer_lager,lager,info],90,5000,[]},
        {[exometer_lager,lager,info],75,5000,[]},
        {[exometer_lager,lager,info],50,5000,[]},
        {[eproc_core,store,get_message],one,10000,[]},
        {[eproc_core,store,get_message],count,10000,[]},
        {[eproc_core,store,attachment_delete],one,10000,[]},    %% New
        {[eproc_core,store,attachment_delete],count,10000,[]}], %% New
        % {[eproc_core,lager,warning],min,20000,[]},            %% Deleted
        % {[eproc_core,lager,warning],mean,20000,[]},           %% Deleted
        % {[eproc_core,lager,warning],max,20000,[]}],           %% Deleted
    NewerExpectedSubs = exometer_report:list_subscriptions(?REPORTER).
