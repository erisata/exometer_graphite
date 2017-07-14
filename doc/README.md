

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


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="exometer_graphite_reporter.md" class="module">exometer_graphite_reporter</a></td></tr></table>

