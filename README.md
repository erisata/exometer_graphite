

# The exometer_graphite application #

This application pushes Exometer metrics to a Graphite server. Also, the
application allows selection of relevant Exometer metrics that are created already
or will be created in future.

Before sending, metrics are stored in configurable time interval buffer and
sent to Carbon (Graphite backend) using Pickle protocol.


### <a name="Configuration">Configuration</a> ###

This application configured in `sys.config` file.

Configuration items:
* host - address of a Graphite server. Default is "localhost"
* port - port of Carbon receiving Pickle protocol messages. Default is 2004.
* send_delay - time interval buffer of storing metrics before sending to Graphite.
Default is 10 seconds.
* connect_timeout - connection to Carbon timeout. In case connection cannot be
established, this application retries specified number of times. After
unsuccessful retries single send_delay interval loop is finished with pending messages for
the next loop. Default is 5 seconds.
* retries - how many times to try sending messages to Graphite in single
send_delay interval loop.
number of retries when sending messages to Carbon on the same socket.
Default is 2.
* resubscription_delay - time interval of renewing subscriptions. New metrics
can be created or deleted. Default is 60 seconds.
* subscriptions - selection of metrics that are wanted to be seen in Graphite. 
Read next section for detail information.


### <a name="Subscription_configuration">Subscription configuration</a> ###


Selecting metrics
is done using Erlang specification matching.

**Note** that subscription entries are scanned in order, and **first match wins**.
In case an application creates new metrics in runtime,
subscriptions are renewed every resubscription_delay,

Top-level generic expresion of a subscription configuration:

```
{subscriptions, [
    subscription(),
    ...
]}
```


#### <a name="subscription()">subscription()</a> ####


```
subscription() = {
                     [
                         {name_pattern(), type()},
                         ...
                     ],
                     datapoint_setting(),
                     interval()
                 }
```


#### <a name="name_pattern()">name_pattern()</a> ####


```
name_pattern() = [name_part()]
```


#### <a name="type()">type()</a> ####

Atom '_' is expression for any.

```
type() = '_' | counter | fast_counter | gauge | histogram | spiral | duration | meter
```


#### <a name="name_part()">name_part()</a> ####

Atom '_' is expression for any.

```
name_part() = '_' | atom() | integer()
```


### <a name="Example_configuration">Example configuration</a> ###


```
    {exometer_graphite, [
        {host, "localhost"},
        {port, 8096},
        {connect_timeout, 5000},
        {send_delay, 3000},
        {retries, 2},
        {resubscription_delay, 1200000},
        {subscriptions, [
            {
                [
                    {[program1,   lager, '_'], '_'},
                    {[program2, lager, '_'], '_'}
                ],
                [mean, max, min],
                20000
            },
            {
                [{[program2, store, '_'], '_'}],
                all,
                10000},
            {
                [{[program3, lager, '_'], '_'}],
                [mean, min, max],
                4000
            },
            {
                [{[program4, lager, '_'], '_'}],
                all,
                5000
            }
        ]}
    ]},
    {exometer_core, [
        {report, [
            {reporters, [
                {exometer_graphite_reporter, []}
            ]}
        ]}
    ]}
```


### <a name="Tips">Tips</a> ###
Following name_pattern() will select all available metrics with name starting
with `program1`:

```
    [program1 | '_']
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_app.md" class="module">exometer_graphite_app</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_reporter.md" class="module">exometer_graphite_reporter</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_subscribers.md" class="module">exometer_graphite_subscribers</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_sup.md" class="module">exometer_graphite_sup</a></td></tr></table>

