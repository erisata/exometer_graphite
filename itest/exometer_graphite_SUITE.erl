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
%%% Common Tests for `exometer_graphite` application.
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
%%  Check, if grouped pickle message sending to mock server is
%%  successful. Using dynamic metric and subscription configuration.
%%
test_message_sending(_Config) ->
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    tcp_server_mock:start(Port, self()),
    exometer:new([testZ, cpuUsage], gauge),
    exometer:new([testB, memUsage], histogram),
    exometer_report:subscribe(?REPORTER, [testZ, cpuUsage], value, 2100, []),
    exometer_report:subscribe(?REPORTER, [testB, memUsage], min, 2000, []),
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
    % checking for <<"server1.*@*.testZ.cpuUsage.value">>
    {Metric1Start1, _} = binary:match(Message, <<"server1.">>),
    {Metric1Start2, _} = binary:match(Message, <<".testZ.cpuUsage.value">>),
    {_, _} = binary:match(Message, <<"@">>, [{scope, {Metric1Start1, Metric1Start2 - Metric1Start1}}]),
    %
    % checking for <<"server1.*@*.testB.memUsage.min">>
    {Metric2Start1, _} = binary:match(Message, <<"server1.">>),
    {Metric2Start2, _} = binary:match(Message, <<".testB.memUsage.min">>),
    {_, _} = binary:match(Message, <<"@">>, [{scope, {Metric2Start1, Metric2Start2 - Metric2Start1}}]).


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
