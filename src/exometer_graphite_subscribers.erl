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
%%% Responsible for managing subscription configuration given in `sys.config'.
%%%
-module(exometer_graphite_subscribers).
-behaviour(gen_server).
-export([
    start_link/0,
    force_resubscribe/0
]).
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-define(REPORTER, exometer_graphite_reporter).
-define(DEFAULT_RESUB_DELAY, 60000).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    resub_delay :: integer()
}).



%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Start a subscription manager.
%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%  @doc
%%  Can be used to initiate subscription update manually.
%%
force_resubscribe() ->
    RequiredSubs = form_required_subs(),
    subscribe(RequiredSubs),
    ok.



%%% ============================================================================
%%% Callbacks for gen_server.
%%% ============================================================================

%% @doc
%% Sets up subscription configuration loop.
%%
init(_) ->
    ResubDelay = exometer_graphite_app:get_env(resub_delay, ?DEFAULT_RESUB_DELAY),
    State = #state{resub_delay = ResubDelay},
    self() ! update,
    {ok, State}.


%% @doc
%% Unused.
%%
handle_call(_Unknown, _From, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
handle_cast(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Updates subscriptions to metrics and continues message sending loop.
%%
handle_info(update, State = #state{resub_delay = ResubDelay}) ->
    erlang:send_after(ResubDelay, self(), update),
    {noreply, State};

handle_info(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc
%% Unused.
%%
terminate(_Reason, _State) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Forms a list of Required Subscriptions.
%%
form_required_subs() ->
    Subs = exometer_graphite_app:get_env(subscriptions, []),
    ReqSubs = lists:foldl(
        fun({Patterns, DatapointSetting, Interval}, Acc) ->
            MatchSpecPatterns = lists:map(
                fun({NamePattern, Type}) ->
                    {{NamePattern, Type, '_'}, [], ['$_']}
                end, Patterns),
            Metrics = exometer:select(MatchSpecPatterns),
            ReqSubsGroup = lists:foldl(
                fun({MetricName, _Type, _State}, SubsGroupAcc) ->
                    SubDatapoints = case DatapointSetting of
                                        all ->
                                            exometer:info(MetricName, datapoints);
                                        Datapoints ->
                                            intersection(Datapoints, exometer:info(MetricName, datapoints))
                                    end,
                    RequiredSubs = [{MetricName, SubDatapoint, Interval} || SubDatapoint <- SubDatapoints],
                    [RequiredSubs|SubsGroupAcc]
                end, [], Metrics),
            [ReqSubsGroup|Acc]
        end, [], Subs),
    lists:flatten(ReqSubs).


%%  @private
%%  Subscribes.
%%  To understand better, I suggest drawing set diagram of ReqSubs, AlreadySubs,
%%  WasteSubs.
%%
subscribe(RequiredSubs) ->
    OldSubs = lists:flatten([{OldMetric, Datapoint}
        || {OldMetric, Datapoint, _Interval, _Extra} <-
            exometer_report:list_subscriptions(?REPORTER)]),
    AlreadySubs = sub_intersection(RequiredSubs, OldSubs),
    NewSubs = RequiredSubs -- AlreadySubs,
    ok = lists:foreach(fun({MetricName, SubDatapoint, Interval}) ->
        exometer_report:subscribe(?REPORTER, MetricName, SubDatapoint, Interval)
                       end, NewSubs),
    WasteSubs = OldSubs -- lists:map(
        fun({MetricName, SubDatapoint, _Interval}) ->
            {MetricName, SubDatapoint}
        end,
        AlreadySubs),
    WasteSubsMetrics = [WasteMetric || {WasteMetric, _Datapoint} <- WasteSubs],
    % removing duplicates
    lists:usort(WasteSubsMetrics),
    lists:foreach(fun(MetricName) ->
        exometer_report:unsubscribe_all(?REPORTER, MetricName)
        end, WasteSubsMetrics),
    ok.


%%  @private
%%  Finds intersecting members of two lists.
%%
sub_intersection(Subs1, Subs2) ->
    [{MetricName, DataPoint, Interval} || {MetricName, DataPoint, Interval} <-
        Subs1, lists:keymember(MetricName, 1, Subs2), lists:keymember(DataPoint, 2, Subs2)].


%%  @private
%%  Finds intersecting members of two lists.
%%
intersection(List1, List2) ->
    [I || I <- List1, lists:member(I, List2)].



%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%%  Check, if intersection is found well.
%%
intersection_test_() ->

    [
        ?_assertEqual([b,c], intersection([a,b,c], [b,c,d])),
        ?_assertEqual("bc", intersection("abc", "bcd")),
        ?_assertEqual([min, max],
            intersection([min, count, low, max], [n,mean,min,max,median]))
    ].


sub_intersection_test_() ->

    [
        ?_assertEqual([{[a,b], mean, 5000}], sub_intersection([{[a,b], mean, 5000}], [{[a,b], mean}])),
        ?_assertEqual([{[a,b], mean, 5000}, {[a,c], max, 10000}], sub_intersection(
            [{[a,b], mean, 5000}, {[a,c], max, 10000}], [{[a,b], mean}, {[a,c], max}])),
        ?_assertEqual([], sub_intersection([{[a,b], mean, 5000}], [{[a,b], min}]))
    ].

-endif.
