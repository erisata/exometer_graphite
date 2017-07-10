-module(basic_testing).
-compile(export_all).

-define(REMOTE_IP, {127, 0, 0, 1}).
-define(REMOTE_PORT, {2004}).

% funkcija, kuri issiuncia hardcoded testine metrika 
send_test_metric() ->
    Payload = pickle:term_to_pickle([{<<"julyXtest.cpuUsage">>, {erlang:system_time(seconds), 1}}]),
    N = byte_size(Payload),
    Message = <<N:32/unsigned-big, Payload/binary>>,

    {ok, Socket} = gen_udp:open(8000),
    ok = gen_udp:send(Socket, {127, 0, 0, 1}, 2004, Message).

populate_test_metrics() ->
    exometer:start(),
    exometer:new([testM], spiral),
    exometer:find_entries([]). 
