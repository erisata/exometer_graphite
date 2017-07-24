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
%%% exometer_graphite_subscribers gen_server supervisor.
%%%
-module(exometer_graphite_subscribers_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Create this supervisor.
%%
start_link() ->
    lager:info("exometer_graphite_subscribers_sup has started!"),
    supervisor:start_link({local, subscribers}, ?MODULE, {}).


%%  @doc
%%  Stop this supervisor.
%%
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.



%%% ============================================================================
%%% Callbacks for supervisor.
%%% ============================================================================

%%  @doc
%%  Supervisor initialization.
%%
init({}) ->
    MaxRestart = 1,
    MaxTime = 3000,
    {ok, {{one_for_all, MaxRestart, MaxTime},
            [{serv,
                {exometer_graphite_subscribers, start_link, []},
                permanent,
                5000, % Shutdown time
                worker,
                [exometer_graphite_subscribers]}]}}.
