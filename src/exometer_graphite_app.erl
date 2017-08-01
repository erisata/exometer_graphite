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
%%% Entry point of the application.
%%%
-module(exometer_graphite_app).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).
-export([name/0, version/0, get_env/1, get_env/2, priv_dir/0]).
-export([start/2, stop/0]).

-define(APP, exometer_graphite).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Returns application name.
%%
-spec name() -> atom().

name() ->
    ?APP.


%%
%%
%%
version() ->
    case lists:keyfind(?APP, 1, application:which_applications()) of
        {_App, _Type, Version}  -> Version;
        false                   -> undefined
    end.


%%
%%
%%
-spec get_env(Name :: atom()) -> undefined | {ok, Value :: term()}.

get_env(Name) ->
    application:get_env(?APP, Name).


%%
%%
%%
-spec get_env(Name :: atom(), Default :: term()) -> Value :: term().

get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).


%%
%%  Returns a path to the file in the application's priv directory.
%%
priv_dir() ->
    case code:priv_dir(?APP) of
        {error, bad_name} -> "priv"; % To allow testing without creating whole app.
        Dir               -> Dir
    end.



%%% ============================================================================
%%% Application callbacks
%%% ============================================================================

%%
%% Start the application.
%%
start(_StartType, _StartArgs) ->
    case exometer_graphite_subscribers_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


%%
%% Stop the application.
%%
stop() ->
    ok.



%%% ============================================================================
%%% Helper functions.
%%% ============================================================================

