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
%%% Common Tests for `exometer_graphite_reporter` module.
%%%
-module(exometer_graphite_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).
-export([
    test_message_sending/1
]).

-define(APP, exometer_graphite).
-define(DEFAULT_TCP_SERVER_MOCK_PORT, 8049).

%%% ============================================================================
%%% Callbacks for `common_test`
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_message_sending
].


%%
%%  Test case test_message_sending initialization.
%%
init_per_testcase(test_message_sending, Config) ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(exometer_graphite),
    [{exometer_graphite_apps, Apps} | Config].


%%
%%  Test case test_message_sending ending.
%%
end_per_testcase(test_message_sending, _Config) ->
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%
%%  Tests if message sending to mock server is
%%  successful. Dynamic metric and subscription configuration.
%%
test_message_sending(_Config) ->
    application:ensure_all_started(exometer_graphite),
    %
    % Port on itest-sys.config, has to be the same as mock server port.
    Port = application:get_env(?APP, port, ?DEFAULT_TCP_SERVER_MOCK_PORT),
    tcp_server_mock:start(Port, self()),
    exometer:new([testZ, cpuUsage], gauge),
    exometer_report:subscribe(exometer_graphite_reporter, [testZ, cpuUsage],
        value, 1000, []),
    Message = receive
        {received, Data} ->
            lager:debug("Successfully received message."),
            Data;
        _Other ->
            lager:debug("Unexpected message.")
    after
        4000 ->
            lager:debug("Did NOT receive message"),
            ok
    end,
    <<Length:(4*8), Info:(6*8), Path:(20*8), Separator:(1*8),
        _Timestamp:(4*8), Ending:(6*8), _OtherMsgs/binary>> = Message,
    TimestampNow = binary:encode_unsigned(erlang:system_time(seconds), little),
    TestMessage = <<0,0,0,37,128,2,93,40,85,20,"testZ.cpuUsage.value",
        74, TimestampNow/binary,75,0,134,134,101,46>>,
    <<TLength:(4*8), TInfo:(6*8), TPath:(20*8),
        TSeparator:(1*8), _TTimestamp:(4*8), TEnding:(6*8)>> = TestMessage,
    true = Length =:= TLength,
    true = Info =:= TInfo,
    true = Path =:= TPath,
    true = Separator =:= TSeparator,
    % Timestamp check ignored
    true = Ending =:= TEnding,
    lager:debug("From tcp: ~p~nFor test: ~p", [Message, TestMessage]).
