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
-module(tcp_server_mock).
-export([
    start/2
]).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Starts a mock server on specific port.
%%
start(LPort, From) ->
    spawn(fun() -> server(LPort, From) end).



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Receives single message and sends it to a process which started the mock server.
%%
server(LPort, From) ->
    {ok, LSock} = gen_tcp:listen(LPort, [{reuseaddr, true},{active, false}, binary]),
    {ok, Sock} = gen_tcp:accept(LSock, 4000),
    {ok, Packet} = gen_tcp:recv(Sock, 0, 4000),
    From ! {received, Packet},
    ok = gen_tcp:close(LSock),
    ok = gen_tcp:close(Sock),
    exit(self(), normal).
