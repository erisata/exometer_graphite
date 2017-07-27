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
%%% Responsible for managing subcription configuration given in
%%% sys.config
%%%
-module(exometer_graphite_subscribers).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
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
-include_lib("exometer_core/include/exometer.hrl").

-define(APP, exometer_graphite).
-define(REPORTER, exometer_graphite_reporter).
-define(DEFAULT_SUBSCRIPTION_DELAY, 10000).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    subscription_delay  :: integer()
}).



%%% ============================================================================
%%% API functions.
%%% ============================================================================

start_link() ->
    lager:debug("Exometer_graphite_subscribers (gen_server) has started."),
    gen_server:start_link(?MODULE, [], []).

force_resubscribe() ->
    lager:debug("FORCED RESUBSCRIPTION"),
    refresh_subscriptions().



%%% ============================================================================
%%% Callbacks for gen_server.
%%% ============================================================================

%% @doc
%% Sets up subscription configuration loop.
%%
init(_) ->
    SubscriptionDelay = application:get_env(?APP, subscription_delay, ?DEFAULT_SUBSCRIPTION_DELAY),
    State = #state{
        subscription_delay = SubscriptionDelay
    },
    self() ! update,
    {ok, State}.


%% @doc
%% Unused.
%%
handle_call(Unknown, _From, State) ->
    lager:warning("Unknown call: ~p", [Unknown]),
    {noreply, State}.


%% @doc
%% Unused.
%%
handle_cast(Unknown, State) ->
    lager:warning("Unknown cast: ~p", [Unknown]),
    {noreply, State}.


%% @doc
%% Updates subscriptions to metrics and continues message sending loop.
%%
handle_info(update, State = #state{subscription_delay = SubscriptionDelay}) ->
    erlang:send_after(SubscriptionDelay, self(), update),
    refresh_subscriptions(),
    {noreply, State};

handle_info(Unknown, State) ->
    lager:warning("Unknown info: ~p", [Unknown]),
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

%% @private
%% Refreshes subscriptions and continues subscription loop.
%%
refresh_subscriptions() ->
    Subs = case application:get_env(?APP, subscriptions, []) of
        SubsFromEnv ->
            SubsFromEnv;
        [] ->
            []
    end,
    [resubscribe(Sub) || Sub <- Subs],
    lager:debug("RESUBSCRIBED"),
    ok.


%%  @private
%%  Resubscribes to a given subscription.
%%
resubscribe({NamePatterns, DatapointSetting, Interval}) ->
    %
    % Select those metrics from exometer that fits our metric name pattern.
    Metrics = exometer:select(NamePatterns),
    [subscribe_to_metric(Metric, DatapointSetting, Interval) || Metric <- Metrics].


%%  @private
%%  Subscribes to an interesting metric.
%%
subscribe_to_metric(Metric, DatapointSetting, Interval) ->
    {MetricName, _Type, _State} = Metric,
    SubDatapoints = case DatapointSetting of
        {all} ->
            exometer:info(MetricName, datapoints);
        {specific, Datapoints} ->
            intersection(Datapoints, exometer:info(MetricName, datapoints))
    end,
    NewSubs = [{MetricName, SubDatapoint} || SubDatapoint <- SubDatapoints],

    OldSubs = lists:flatten([{{OldMetric, Datapoint}, Interval}
        || {OldMetric, Datapoint, Interval, _Extra} <- exometer_report:list_subscriptions(?REPORTER)]),
    [subscribe(NewSub, OldSubs, Interval) || NewSub <- NewSubs].


%%  @private
%%  Subscribes
%%
subscribe(_NewSub = {MetricName, SubDatapoint}, OldSubs, Interval) ->
    case lists:keysearch({MetricName, SubDatapoint}, 1, OldSubs) of
        {value, {_Key, OldInterval}} ->
            case OldInterval of
                Interval ->
                    ok;
                _ ->
                    exometer_report:unsubscribe(?REPORTER, MetricName, SubDatapoint),
                    exometer_report:subscribe(?REPORTER, MetricName, SubDatapoint, Interval)
            end;
        false ->
            exometer_report:subscribe(?REPORTER, MetricName, SubDatapoint, Interval)
    end.














%%    ThisOneSubscription = {MetricName, SubDatapoints, Interval},
%%    OldSubs = exometer_report:list_subscriptions(?REPORTER),
%%    % TODO: scenario where existing subscription with many datapoint should be
%%    % overriden by subscription with less datapoints.
%%    case intersection([ThisOneSubscription], OldSubs) of
%%        [] ->
%%            lager:debug("New subscribtion"),
%%            exometer_report:subscribe(exometer_graphite_reporter, MetricName, SubDatapoints, Interval),
%%            resubscribed;
%%        _ ->
%%            lager:debug("Not resubscribing"),
%%            not_resubscribed
%%    end.


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
find_intersection_test_() ->

    [
        ?_assertEqual([b,c], intersection([a,b,c], [b,c,d])),
        ?_assertEqual("bc", intersection("abc", "bcd")),
        ?_assertEqual([min, max],
            intersection([min, count, low, max], [n,mean,min,max,median]))
    ].

%%  TODO: Multiline eunit test for 'subscribe_to_interesting_metric'



-endif.
