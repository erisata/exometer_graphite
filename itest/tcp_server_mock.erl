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
%%% Used to mock a TCP server. For example, Graphite server
%%%
-module(tcp_server_mock).
-compile([{parse_transform, lager_transform}]).
-export([
    start/2
]).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%% @doc
%% Starts a mock server on specific port.
%%
start(ListenPort, From) ->
    case gen_tcp:listen(ListenPort, [{active, true}, binary]) of
        {ok, ListenSocket} ->
            lager:debug("Listening socket opened."),
            spawn(fun() -> server(ListenSocket, From) end),
            {ok, Port} = inet:port(ListenSocket),
            Port;
        {error,Reason} ->
            lager:debug("Could not open listening socket."),
            {error,Reason}
    end.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%% @private
%% Waits for TCP connection.
%%
server(ListenSocket, From) ->
    case gen_tcp:accept(ListenSocket, 5000) of
        {ok, AcceptedSocket} ->
            loop(AcceptedSocket, From),
            ok = gen_tcp:close(AcceptedSocket);
        {error, timeout} ->
            lager:warning("Timeout: no connection in 5 seconds."),
            ok;
        _Other ->
            ok
    end,
    ok = gen_tcp:close(ListenSocket).



%% @private
%% Receives data sent via TCP.
%%
loop(AcceptedSocket, From) ->
    receive
        {tcp, AcceptedSocket, Data} ->
            % Could reply using gen_tcp:send(S,Answer),
            From ! {received, Data},
            % Receives only single message
            loop(AcceptedSocket, From);
        {tcp_closed, AcceptedSocket} ->
            ok
    end.
