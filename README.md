

# The exometer_graphite application #

This application provides Exometer Core and Graphite integration.
It can be integrated to an application which pushes metric data to
Exometer Core.

Subscriptions to metrics can be set up in sys.config file using Erlang
specification matching or specifying full metric path.

To setup Graphite follow this guide until statsd configuration:
https://linoxide.com/ubuntu-how-to/setup-graphite-statsd-ubuntu-16-04/

To try out this application in stand-alone configure sys.config like this:

```
{exometer_core, [
        {report, [
            {reporters, [
                {exometer_graphite_reporter, []}
            ]},
            {subscribers, [
                {exometer_graphite_reporter, [testZ, cpuUsage], value, 3000, true},
                {exometer_graphite_reporter, [testZ, memUsage], value, 5000, true}
            ]}
        ]}
    ]}
```

And run these commands:

```
$ make deps
$ env ERL_LIBS=deps erl -pa ebin/ -config test/sys
> application:ensure_all_started(exometer_graphite).
> exometer:start().
> exometer:new([testZ, cpuUsage], gauge).
```

## Expected usage ##
Suppose we have a project that pushes metrics to exometer by using
`exometer:update_or_create/4'.

In order to use this project, we add it as a dependency in rebar.config.
Also, we add this application to our application's `.app.src'

In our sys.config we add configuration for this application (exometer_graphite).
Here we define timings and subscriptions to metrics.

Note that if a subscriptions in sys.config are overriden. Which means that, a
subscription for metric name [a, b, c], datapoint min and interval 10000 will
be overriden by metric name [a, b, c], datapoint min and interval 3000, if it is
lower in the file.

Subscription format is list of subscriptions. Each subscription has a list of
match_spec patterns for choosing metrics, tuple describing datapoints (for all
datapoints of metrics use all) and interval.

Example:
```
{subscriptions, [
            {[{ {[testHis, lager, '_'], '_', '_'}, [], ['$_']},
                { {[eproc_core, store, '_'], '_', '_'}, [], ['$_']}
            ], {all}, 10000},
            {[{ {[testGauge],'_','_'}, [], ['$_']}],
                {specific, [count, one]}, 8000}
        ]}
```

Lets say our solution is running and we can't restart it and we want to change
subscriptions dynamically. This way we can change
sys.config file ant run exometer_graphite_subscribers:force_resubscribe/0 or
wait for automatic resubscription. Automatic resubscription allows to subscribe
to the metrics that will exist in future.

## Functional requirements ##
1. Subscriptions are configured statically.
    1. Subscribing to metric names is done by provinding a pattern.
    1. Possible to subscribe to specific datapoints.
    1. Possible to subscribe to all datapoints of the metric.
1. Possible to subscribe to the metrics that are not yet existing.
1. Possible to configure resubscription interval.
1. Metrics are aggregated and sent to Graphite.
1. Interval of metric sending to Graphite is configurable.
1.

## Non-functional requirements ##
1.

## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_reporter.md" class="module">exometer_graphite_reporter</a></td></tr></table>

