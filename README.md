

# The exometer_graphite application #

This application provides Exometer Core and Graphite integration.
It can be added as a dependency to an application which pushes metric
data to Exometer Core.

Subscriptions to metrics can be set up in sys.config file using Erlang
specification matching.

To setup Graphite on Ubuntu 16.04 follow this guide until statsd configuration:
https://linoxide.com/ubuntu-how-to/setup-graphite-statsd-ubuntu-16-04/

To setup Graphite on OpenSUSE or SLES, get graphite-inst from Erisata. Build
.rpm files and install them. Graphite-inst will be released in August, 2017.

## Important notes ##
* Subscription entries are scanned in order, and **first match wins**.
Subscribtions are renewed every subscription_delay,
in case application creates new metrics in runtime.


## Running in stand-alone mode ##
To try out this application in stand-alone configure sys.config like this:
```
[
    {exometer_graphite, [
        {host, "localhost"},
        {port, 2004},
        {connection_timeout, 5000},
        {graphite_delay, 3000},
        {subscription_delay, 1200000},
        {subscriptions, [
            {[{ {[testHis, lager], '_', '_'}, [], ['$_']},
                { {[eproc_core, store, '_'], '_', '_'}, [], ['$_']}
            ], {all}, 10000},
            {[{ {[testGauge],'_','_'}, [], ['$_']}],
                {specific, [count, one]}, 8000}
        ]}
    ]},
    {exometer_core, [
        {report, [
            {reporters, [
                {exometer_graphite_reporter, []}
            ]}
        ]}
    ]}
].
```

And run these commands:

```
$ make deps
$ env ERL_LIBS=deps erl -pa ebin/ -config test/sys
> application:ensure_all_started(exometer_graphite).
> exometer:start().
> exometer:new([testZ, cpuUsage], gauge).
```
Open localhost in browser and you should see zero line graph in exometer
when metric testZ.cpuUsage.gauge is selected.

## Running as a dependency in another project ##
Suppose we have a project that pushes metrics to exometer by using
`exometer:update_or_create/4'.

In order to use this project, we add it as a dependency in rebar.config.
Also, we add this application to your application's `.app.src'

In our sys.config we add configuration for this application (exometer_graphite)
and define timings and subscriptions to metrics.

**Note** that subscriptions in sys.config are overriden. When subscribing
to the same metric and datapoint. Which means that, a
subscription for metric name \[a, b, c], datapoint min and interval 10000 will
be overriden by metric name \[a, b, c], datapoint min and interval 3000, if it is
lower in the file.

Subscription format is list of subscriptions. Each subscription has a list of
match_spec patterns for choosing metrics, tuple describing datapoints (for all
datapoints of metrics use all) and interval.

Example of static subscription configuration:
```
{subscriptions, [
            {[{ {[axb, esb, ad], '_', '_'}, [], ['$_']},
                { {[axb, esb, fm], '_', '_'}, [], ['$_']},
                { {[axb, esb, fm, '_'], '_', '_'}, [], ['$_']}
            ], {all}, 10000},
            {[{ {[esb, exometer_lager, lager, '_'], '_', '_'}, [], ['$_']}
            ], {specific, [max, mean, median, min, n]}, 5000}
        ]}
```

Lets say our solution is running and we can't restart it and we want to change
subscriptions dynamically. This way we can change
sys.config file ant run exometer_graphite_subscribers:force_resubscribe/0 or
wait for automatic resubscription. Automatic resubscription allows to subscribe
to the metrics that will exist in future.


## Functional requirements ##
1. Subscriptions are configured statically.
    1. Subscribing to metric names is done by providing a pattern.
    1. Possible to subscribe to specific datapoints.
    1. Possible to subscribe to all datapoints of the metric.
1. Possible to subscribe to the metrics that are not yet existing.
1. Possible to configure resubscription interval.
1. Interval of metric sending to Graphite is configurable.


## Non-functional requirements ##
1. Metrics are grouped and then sent to Graphite.
1. An existing subscription is not overriden by identical subscription.

## TODO ##
Change idea project to erlang

## Modules ##
<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_app.md" class="module">exometer_graphite_app</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_reporter.md" class="module">exometer_graphite_reporter</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_subscribers.md" class="module">exometer_graphite_subscribers</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_subscribers_sup.md" class="module">exometer_graphite_subscribers_sup</a></td></tr>
</table>

