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
    lager:info("INIT STARTED"),
    SubscriptionDelay = application:get_env(?APP, subscription_delay, ?DEFAULT_SUBSCRIPTION_DELAY),
%%    lager:info("SubscriptionSpecs: ~p", [Subscriptions]),
    exometer:new([eproc_core, store, get_message], spiral),
%%    exometer:new([eproc_core, store, attachment_save], spiral),
%%    exometer:new([eproc_core, something, attachment_load], spiral),
%%    exometer:new([eproc_core, something, attachment_save], spiral),


    State = #state{
        subscription_delay = SubscriptionDelay
    },

    self() ! update,
    lager:info("INIT FINISHED"),
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
%% Updates subscribtions to metrics and continues message sending loop.
%%
handle_info(update, State) ->
    #state{
        subscription_delay = SubscriptionDelay
    } = State,
    erlang:send_after(SubscriptionDelay, self(), update),
    refresh_subscriptions(),

%%    lager:info("Subscriptions of my reporter: ~p", [exometer_report:list_subscriptions(?REPORTER)]),

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

%% @doc
%% Refreshes subscriptions and continues subscription loop.
%%
refresh_subscriptions() ->
    Subscriptions = case application:get_env(?APP, subscriptions, []) of
        {ok, SubscriptionsFromEnv} ->
            SubscriptionsFromEnv;
        [] ->
            []
    end,

%%    {ok, Subscriptions} = application:get_env(?APP, subscriptions, []),

    OldSubscriptions = exometer_report:list_subscriptions(?REPORTER),

    [exometer_report:unsubscribe_all(exometer_graphite_reporter, MetricName)
        || {MetricName, _DataPoint, _Interval, _Extra} <- OldSubscriptions],

    [resubscribe(Subscription) || Subscription <- Subscriptions],
    lager:info("RESUBSCRIBED!!!"),
    ok.


%% @doc
%% Resubscribes to single subscription.
%%
resubscribe(Subscription) ->
    {select, {MatchPattern, DataPoint, Interval}} = Subscription,
%%    lager:info("MatchPattern: ~p", [MatchPattern]),

    Metrics = exometer:select(MatchPattern), % get already defined metrics in exometer

    % issitraukiu, kokias metrikas jau turiu uzsisubscribines
    % antra karta ant tu paciu metriku nelipu
    % TODO: THIS IS FAULTY
    [exometer_report:subscribe(?REPORTER, Name, DataPoint, Interval, [], false)
        || {Name, _Type, _Status} <- Metrics],
    exometer_report:restart_intervals(?REPORTER),
    ok.

%% pseudo resubscribe
%% Metrics = exometer:select(MatchPattern),
%% jeigu toks pat {select, {Match...}} ir vienas is list_subscriptions

%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================
