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
    [exometer_report:unsubscribe_all(?REPORTER, Metric)
        || {Metric, _, _, _} <- exometer_report:list_subscriptions(?REPORTER)],
    ok;

end_per_testcase(test_static_configuration, _Config) ->
    [exometer_report:unsubscribe_all(?REPORTER, Metric)
        || {Metric, _, _, _} <- exometer_report:list_subscriptions(?REPORTER)],
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%  Tests if message sending to mock server is
%%  successful. Dynamic metric and subscription configuration.
%%
test_message_sending(_Config) ->
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    tcp_server_mock:start(Port, self()),
    exometer:new([testZ, cpuUsage], gauge),
    exometer_report:subscribe(?REPORTER, [testZ, cpuUsage],
        value, 1000, []),
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
    <<Length:(4*8), Info:(6*8), Path:(20*8), Separator:(1*8),
        _Timestamp:(4*8), Ending:(6*8), _OtherMsgs/binary>> = Message,
    TimestampNow = binary:encode_unsigned(erlang:system_time(seconds), little),
    TestMessage = <<0,0,0,37,128,2,93,40,85,20,"testZ.cpuUsage.value",
        74, TimestampNow/binary,75,0,134,134,101,46>>,
    <<TLength:(4*8), TInfo:(6*8), TPath:(20*8),
        TSeparator:(1*8), _TTimestamp:(4*8), TEnding:(6*8)>> = TestMessage,
    true = Length =:= TLength,
    true = Info =:= TInfo,
    true = Path =:= TPath,
    true = Separator =:= TSeparator,
    % Timestamp check ignored
    true = Ending =:= TEnding,
    lager:debug("From tcp: ~p~nFor test: ~p", [Message, TestMessage]).


%%  @doc
%%  Check, if static subscriptions are passed to exometer report.
%%
test_static_configuration(_Config) ->
    timer:sleep(500),
    %
    exometer:new([eproc_core, lager, warning], histogram),
    exometer:new([eproc_core, store, get_message], spiral),
    %
    ok = exometer:update_or_create([testEL, lager, debug            ], 1, histogram, []),
    ok = exometer:update_or_create([exometer_lager, lager, info     ], 1, histogram, []),
    %
    % Forcing resubscribe, though it would be automatic resubscription by interval.
    ok = exometer_graphite_subscribers:force_resubscribe(),
    timer:sleep(200),
    ExpectedSubs = [
        {[testEL,lager,debug],min,4000,[]},
        {[testEL,lager,debug],mean,4000,[]},
        {[testEL,lager,debug],max,4000,[]},{[exometer_lager,lager,info],n,5000,[]},
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
    ActualSubs = exometer_report:list_subscriptions(exometer_graphite_reporter),
    true = ActualSubs =:= ExpectedSubs,
    %
    % Checking a situation when a new metric fitting subscription is added.
    exometer:new([eproc_core, store, attachment_delete], spiral),
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
        {[eproc_core,store,attachment_delete],one,10000,[]},    %% NEWLY ADDED
        {[eproc_core,store,attachment_delete],count,10000,[]},  %% NEWLY ADDED
        {[eproc_core,lager,warning],min,20000,[]},
        {[eproc_core,lager,warning],mean,20000,[]},
        {[eproc_core,lager,warning],max,20000,[]}],
    NewerActualSubs = exometer_report:list_subscriptions(?REPORTER),
    true = NewerActualSubs =:= NewerExpectedSubs.
