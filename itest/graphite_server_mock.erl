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
%%% Used to mock a TCP server, which receives single message.
%%%
-module(graphite_server_mock).
-compile([{parse_transform, lager_transform}]).
-export([start/2, stop/1]).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Starts a mock server on specific port.
%%
start(LPort, From) ->
    Pid = spawn_link(fun() -> server(LPort, From) end),
    {ok, Pid}.


%%  @doc
%%  Stop the mock server.
%%
stop(Pid) ->
    Pid ! stop,
    receive
        stopped         -> ok;
        {error, Reason} -> {error, Reason}
    after
        5000 -> {error, timeout}
    end.


%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Receives single message and sends it to a process which started the mock server.
%%
server(LPort, From) ->
    {ok, LSock} = gen_tcp:listen(LPort, [{active, true}, {reuseaddr, true}, binary]),
    {ok, Sock} = gen_tcp:accept(LSock, 4000),
    receive
        {tcp, Sock, Packet} ->
            From ! {received, Packet},
            receive
                stop ->
                    ok = gen_tcp:shutdown(Sock, write),
                    receive
                        {tcp_closed, Sock} ->
                            ok = gen_tcp:close(Sock),
                            ok = gen_tcp:close(LSock),
                            From ! stopped
                    after
                        5000 ->
                            From ! {error, close_timeout}
                    end
            end
        after
            5000 ->
                {error, timeout}
    end.


