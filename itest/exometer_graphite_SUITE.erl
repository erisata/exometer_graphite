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
%%    test_message_sending,
    test_static_configuration
].

%%  TODO: use init_per_suite instead of init_per_testcase or use
%%  init_per_testcase(TestCase, Config) ->
%%
%%  Test case test_message_sending initialization.
%%
init_per_testcase(test_message_sending, Config) ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(exometer_graphite),
    [{exometer_graphite_apps, Apps} | Config];

init_per_testcase(test_static_configuration, Config) ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(exometer_graphite),
    [{exometer_graphite_apps, Apps} | Config].


%%
%%  Test case test_message_sending ending.
%%
end_per_testcase(test_message_sending, _Config) ->
    ok;

end_per_testcase(test_static_configuration, _Config) ->
    ok.


%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  TODO: remove debug logs for full messages
%%  @doc
%%  Tests if message sending to mock server is
%%  successful. Dynamic metric and subscription configuration.
%%
test_message_sending(_Config) ->
    application:ensure_all_started(exometer_graphite),
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    tcp_server_mock:start(Port, self()),
    exometer:new([testZ, cpuUsage], gauge),
    exometer_report:subscribe(exometer_graphite_reporter, [testZ, cpuUsage],
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
%%  Check, if static subscriptions are passed to exometer.
%%
test_static_configuration(_Config) ->
    application:load(exometer_graphite),
    application:set_env(exometer_graphite, subscriptions, [
        {[{ {[axb_core, lager, '_'], '_', '_'}, [], ['$_']},
            { {[eproc_core, lager, '_'], '_', '_'}, [], ['$_']}
        ], {specific, [mean, max, min]}, 20000},
        {[{ {[eproc_core, store, '_'], '_', '_'}, [], ['$_']}
        ], {all}, 10000}
    ]),
    application:ensure_all_started(exometer_graphite),
    timer:sleep(2000),
    %
    exometer:new([axb_core, lager, error], histogram),
    exometer:new([eproc_core, lager, warning], histogram),
    exometer:new([eproc_core, store, get_message], spiral),
    exometer:new([eproc_core, store, attachment_save], spiral),
    exometer:new([eproc_core, store, attachment_load], spiral),
    exometer:new([testHis, lager], histogram),
    %
    % Forcing resubscribe, though it would be automatic resubscription interval.
    ok = exometer_graphite_subscribers:force_resubscribe(),
    timer:sleep(200),
    ExpectedSubs = [
        {[eproc_core,store,get_message],[count,one],10000,[]},
        {[eproc_core,store,attachment_save],[count,one],10000,[]},
        {[eproc_core,store,attachment_load],[count,one],10000,[]},
        {[eproc_core,lager,warning],[mean,max,min],20000,[]},
        {[axb_core,lager,error],[mean,max,min],20000,[]}],
    ActualSubs = exometer_report:list_subscriptions(exometer_graphite_reporter),
    true = ActualSubs =:= ExpectedSubs,
    %
    % Checking a situation when a new metric fitting subscription is added.
    exometer:new([eproc_core, store, attachment_delete], spiral),
    ok = exometer_graphite_subscribers:force_resubscribe(),
    timer:sleep(200),
    %
    % If same subscription is added to exometer twice, only one will be in
    % exometer_report:list_subscriptions,
    NewerExpectedSubs = [
        {[eproc_core,store,get_message],[count,one],10000,[]},
        {[eproc_core,store,attachment_save],[count,one],10000,[]},
        {[eproc_core,store,attachment_load],[count,one],10000,[]},
        {[eproc_core,store,attachment_delete],[count,one],10000,[]},
        {[eproc_core,lager,warning],[mean,max,min],20000,[]},
        {[axb_core,lager,error],[mean,max,min],20000,[]}],
    NewerActualSubs = exometer_report:list_subscriptions(exometer_graphite_reporter),
    true = NewerActualSubs =:= NewerExpectedSubs,
    %
    lager:debug("Subs now: ~p", [exometer_report:list_subscriptions(exometer_graphite_reporter)]).


%%  @doc
%%  Check, if static subscriptions are passed to exometer.
%%
%%test_resubscription_duplicates(_Config) ->