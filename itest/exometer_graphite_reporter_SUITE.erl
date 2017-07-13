%%%
%%% Testcases for `exometer_graphite_reporter` module.
%%%
-module(exometer_graphite_reporter_SUITE).
-compile([{parse_transform, lager_transform}]).

%% API
-export([]).


%%% ============================================================================
%%% Testcases.
%%% ============================================================================

test_initialization(_Config) ->
    application:ensure_all_started(exometer_graphite),
    %%exometer:start(),

    % Configure MY reporter to send to MOCK server (or just take over 2004 port
%       (stop carbon) by hands or send to another port)

    % start Exometer
    % add MY reporter
    % add metric
    % subscribe to metric

    % connect to MOCK serv

    % MOCK server waiting for Message
    % MOCK server receives a message PRINT IT

    % MOCK server passes the message to Common Test
    % Common Test checks the message


