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
%%% Common Tests for `exometer_graphite' application.
%%%
-module(exometer_graphite_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_message_sending/1,
    test_static_configuration/1
]).

-define(APP, exometer_graphite).
-define(DEFAULT_TCP_SERVER_MOCK_PORT, 8049).
-define(REPORTER, exometer_graphite_reporter).

%%% ============================================================================
%%% Callbacks for `common_test'
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_message_sending,
    test_static_configuration
].


%%
%%  CT API, initialization.
%%
init_per_suite(Config) ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(?APP),
    [{exometer_graphite_apps, Apps} | Config].


%%
%%  CT API, cleanup.
%%
end_per_suite(Config) ->
    ok = lists:foreach(
        fun (A) -> application:stop(A) end,
        proplists:get_value(exometer_graphite_apps, Config)
    ),
    ok.


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("---------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end. Also, clean subscriptions and metrics.
%%
end_per_testcase(TestCase, _Config) ->
    lists:foreach(fun({Metric, _, _, _}) ->
        exometer_report:unsubscribe_all(?REPORTER, Metric)
                  end, exometer_report:list_subscriptions(?REPORTER)),
    lists:foreach(fun({Name, _Type, _Status}) ->
        exometer:delete(Name)
                  end, exometer:find_entries([])),
    lager:debug("---------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%  Check, if static subscriptions are passed to exometer report.
%%  Check, if a future subscription is working when its metric is added.
%%  Check, if subscription is removed if a metric is removed.
%%  Check, if automatic resubscription works.
%%
test_static_configuration(_Config) ->
    exometer:new([testA, cpuUsage], gauge),
    exometer:new([testB, memUsage], histogram),
    exometer:new([testC, lager, warning], histogram), % Should be ignored.
    exometer:new([testC, store, get_message], spiral),
    ok = exometer:update_or_create([testD, lager, info], 1, histogram, []),
    %
    % Waiting for an automatic resubscription.
    timer:sleep(2100),
    ExpectedSubs = [
        {[testD,lager,info],n,4000,[]},
        {[testD,lager,info],min,4000,[]},
        {[testD,lager,info],median,4000,[]},
        {[testD,lager,info],mean,4000,[]},
        {[testD,lager,info],max,4000,[]},
        {[testD,lager,info],999,4000,[]},
        {[testD,lager,info],99,4000,[]},
        {[testD,lager,info],95,4000,[]},
        {[testD,lager,info],90,4000,[]},
        {[testD,lager,info],75,4000,[]},
        {[testD,lager,info],50,4000,[]},
        {[testC,store,get_message],one,10000,[]},
        {[testC,store,get_message],count,10000,[]},
        {[testB,memUsage],min,2000,[]},
        {[testA,cpuUsage],value,2000,[]}],
    ExpectedSubs = exometer_report:list_subscriptions(exometer_graphite_reporter),
    %
    % Checking a situation when a new metric fitting subscription is added.
    ok = exometer:update_or_create([testE, lager, debug], 1, histogram, []),
    ok = exometer:delete([testC, store, get_message]),
    %
    % Waiting for an automatic resubscription.
    timer:sleep(2100),
    %
    % If same subscription is added to exometer twice, only one will be shown in
    % exometer_report:list_subscriptions, though actually there would be two.
    NewExpectedSubs = [
        {[testE,lager,debug],min,5000,[]},  %% New
        {[testE,lager,debug],mean,5000,[]}, %% New
        {[testE,lager,debug],max,5000,[]},  %% New
        {[testD,lager,info],n,4000,[]},
        {[testD,lager,info],min,4000,[]},
        {[testD,lager,info],median,4000,[]},
        {[testD,lager,info],mean,4000,[]},
        {[testD,lager,info],max,4000,[]},
        {[testD,lager,info],999,4000,[]},
        {[testD,lager,info],99,4000,[]},
        {[testD,lager,info],95,4000,[]},
        {[testD,lager,info],90,4000,[]},
        {[testD,lager,info],75,4000,[]},
        {[testD,lager,info],50,4000,[]},
        % {[testC,store,get_message],one,10000,[]},   %% Deleted
        % {[testC,store,get_message],count,10000,[]}, %% Deleted
        {[testB,memUsage],min,2000,[]},
        {[testA,cpuUsage],value,2000,[]}],
    NewExpectedSubs = exometer_report:list_subscriptions(?REPORTER).


%%  @doc
%%  Check, if grouped pickle message sending to mock server is successful.
%%
test_message_sending(_Config) ->
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    {ok, MockPid} = graphite_server_mock:start(Port, self()),
    exometer:new([testA, cpuUsage], gauge),
    exometer:new([testB, memUsage], histogram),
    exometer_graphite_subscribers:force_resubscribe(),
    exometer:update([testB, memUsage], 10),
    Message = receive
        {received, Data} ->
            Data;
        _Other ->
            lager:debug("Unexpected message.")
    after
        4000 ->
            lager:debug("Did NOT receive message"),
            ok
    end,
    %
    % node@host differs from system to system
    % checking for <<"server1.*@*.testA.cpuUsage.value">>
    {Metric1Start1, _} = binary:match(Message, <<"server1.">>),
    {Metric1Start2, _} = binary:match(Message, <<".testA.cpuUsage.value">>),
    {_, _} = binary:match(Message, <<"@">>, [{scope, {Metric1Start1, Metric1Start2 - Metric1Start1}}]),
    %
    % checking for <<"server1.*@*.testB.memUsage.min">>
    {Metric2Start1, _} = binary:match(Message, <<"server1.">>),
    {Metric2Start2, _} = binary:match(Message, <<".testB.memUsage.min">>),
    {_, _} = binary:match(Message, <<"@">>, [{scope, {Metric2Start1, Metric2Start2 - Metric2Start1}}]),
    ok = graphite_server_mock:stop(MockPid),
    ok.


