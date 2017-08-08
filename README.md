

# The exometer_graphite application #

__Authors:__ Kazimieras Senvaitis ([`kazimieras.senvaitis@erisata.lt`](mailto:kazimieras.senvaitis@erisata.lt)).


### <a name="Contents">Contents</a> ###
1. [Introduction](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#Introduction)
1. [Graphite setup](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#Graphite_setup)
1. [Usage](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#Usage)
1. [System Requirement Specification](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#System_Requirement_Specification)
1. [Developer notes](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#Developer_notes)
1. [TODO](http://github.com/erisata/exometer_graphite/blob/master/doc/README.md#TODO)



### <a name="Introduction">Introduction</a> ###

This application provides Exometer Core and Graphite integration.
It can be added as a dependency to an application which pushes metric
data to Exometer Core.


### <a name="Static_subscription_configuration">Static subscription configuration</a> ###

**Note** that subscription entries are scanned in order, and **first match wins**.
Subscribtions are renewed every subscription_delay,
in case application creates new metrics in runtime.

Top-level generic expresion of a subscription configuration:

```
{subscriptions, [
            [subscription(), ...]}
```


#### <a name="subscription()">subscription()</a> ####


```
subscription() = {[{name_pattern(), type()},
                    ...],
                    datapoint_setting(), interval()}
```



#### <a name="name_pattern()">name_pattern()</a> ####


```
name_pattern() = list()
```



#### <a name="type()">type()</a> ####


```
type() = '_' | counter | fast_counter | gauge | histogram | spiral | duration | meter
```


#### <a name="Examples">Examples</a> ####



<table border="1" summary="reference syntax"><tr><th>Metric name</th><th>Type</th><th>Datapoints</th><th>Interval</th><th>Subscription</th></tr><tr><td>[axb, esb, ad]</td><td>*</td><td>*</td><td>*</td><td>10000</td><td>
<pre>{[{ {[axb, esb, ad], '_', '_'}, [], ['$_']}],
    {all}, 10000}</pre></td></tr>
</table>



### <a name="Usage">Usage</a> ###

Subscriptions to metrics can be set up in sys.config file using Erlang
specification matching.


#### <a name="Running_in_stand-alone_mode">Running in stand-alone mode</a> ####

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


#### <a name="Running_as_a_dependency_in_another_project">Running as a dependency in another project</a> ####

Suppose we have a project that pushes metrics to exometer by using
`exometer:update_or_create/4`.

In order to use this project, we add it as a dependency in rebar.config.
Also, we add this application to your application's `.app.src`

In our sys.config we add configuration for this application (exometer_graphite)
and define timings and subscriptions to metrics.

Subscription format is list of subscriptions. Each subscription has a list of
match_spec patterns for choosing metrics, tuple describing datapoints (for all
datapoints of metrics use all) and interval.

Example of static subscription configuration:

```
{subscriptions, [
            {[ {[axb_core, lager, '_'], '_'},
                { [eproc_core, lager, '_'], '_'}],
                [mean, max, min], 20000},
            {[ {[eproc_core, store, '_'], '_'}],
                all, 10000},
            {[ {[testEL, lager, '_'], '_'}],
                [mean, min, max], 4000},
            {[ {[exometer_lager, lager, '_'], '_'}],
                all, 5000}
        ]}
```

Lets say our solution is running and we can't restart it and we want to subscribe
to newly created metrics. To do that we can change run
`exometer_graphite_subscribers:force_resubscribe/0` or
wait for **automatic resubscription**. So, automatic resubscription allows to subscribe
to the metrics that will exist in future.


### <a name="System_Requirement_Specification">System Requirement Specification</a> ###


#### <a name="Functional_requirements">Functional requirements</a> ####

1. Subscriptions are configured statically.
1. Subscribing to metric names is done by providing a pattern.
1. Possible to subscribe to specific datapoints.
1. Possible to subscribe to all datapoints of the metric.
1. Possible to subscribe to the metrics that are not yet existing.
1. Possible to configure resubscription interval.
1. Interval of metric sending to Graphite is configurable.
1. Possible to subscribe to all metrics of an application  with single subscription line.


#### <a name="Non-functional_requirements">Non-functional requirements</a> ####

1. Metrics are grouped and then sent to Graphite.
1. An existing subscription is not overriden by identical subscription.
1. Application does not crash if graphite server unavailable.
1. Application does not crash if static configuration produces error.


### <a name="Developer_notes">Developer notes</a> ###

* Subscribing twice to the same metric makes two subscriptions. Though
exometer_report:list_subscription/X shows only the newer subscription.


### <a name="TODOs">TODOs</a> ###

Change idea project to erlang


### <a name="Graphite_setup">Graphite setup</a> ###

To setup Graphite on Ubuntu 16.04 follow this guide until statsd configuration:
https://linoxide.com/ubuntu-how-to/setup-graphite-statsd-ubuntu-16-04/

To setup Graphite on OpenSUSE or SLES, get graphite-inst from Erisata. Build
.rpm files and install them. Graphite-inst will be released in August, 2017.


### <a name="Setup_Graphite_0.9.10_on_SLES_11_SP4">Setup Graphite 0.9.10 on SLES 11 SP4</a> ###


#### <a name="Steps">Steps</a> ####

```
cd /opt/graphite/webapp/graphite/
cp local_settings.py.example local_settings.py

cd /opt/graphite/conf/
cp graphite.wsgi.example graphite.wsgi
cp storage-aggregation.conf.example storage-aggregation.conf
cp storage-schemas.conf.example storage-schemas.conf
cp carbon.conf.example carbon.conf

cd /var/run
mkdir apache2

cd /opt/graphite/webapp/graphite/
python manage.py syncdb

cp /opt/graphite/examples/example-graphite-vhost.conf /etc/apache2/vhosts.d/

chmod
```

Files needed:

Run: sudo python setup.py install
* graphite-web-0.9.10.tar.gz
* whisper-0.9.10.tar.gz
* carbon-0.9.10.tar.gz

* django-tagging-0.3.1.tar.gz
* Django-1.1.4.tar.gz

Run: sudo zypper in
* python-Twisted-12.0.0-10.1.x86_64.rpm

YaST2:
* apache2 2.2.34
* apache2-mod_wsgi 4.4.13


#### <a name="Configuration">Configuration</a> ####


#### <a name="local_settings.py">local_settings.py</a> ####

/opt/graphite/webapp/graphite/local_settings.py

```
TIME_ZONE = 'Europe/Vilnius'
```


<h5><a name="Virtual_host">Virtual host</a></h5>

```
# This configuration assumes the default installation prefix
# of /opt/graphite/, if you installed graphite somewhere else
# you will need to change all the occurances of /opt/graphite/
# in this file to your chosen install location.
<IfModule !wsgi_module.c>
    LoadModule wsgi_module modules/mod_wsgi.so
</IfModule>

# XXX You need to set this up!
# Read http://code.google.com/p/modwsgi/wiki/ConfigurationDirectives#WSGISocketPrefix
WSGISocketPrefix /var/run/apache2/wsgi

<VirtualHost *:80>
        ServerName graphite
        DocumentRoot "/opt/graphite/webapp"
        ErrorLog /opt/graphite/storage/log/webapp/error.log
        CustomLog /opt/graphite/storage/log/webapp/access.log common

        # I've found that an equal number of processes & threads tends
        # to show the best performance for Graphite (ymmv).
        WSGIDaemonProcess graphite processes=5 threads=5 display-name='%{GROUP}' inactivity-timeout=120
        WSGIProcessGroup graphite
        WSGIApplicationGroup %{GLOBAL}
        WSGIImportScript /opt/graphite/conf/graphite.wsgi process-group=graphite application-group=%{GLOBAL}

        # XXX You will need to create this file! There is a graphite.wsgi.example
        # file in this directory that you can safely use, just copy it to graphite.wgsi
        WSGIScriptAlias / /opt/graphite/conf/graphite.wsgi

        Alias /content/ /opt/graphite/webapp/content/
        <Location "/content/">
                SetHandler None
                Allow from all
        </Location>

        # XXX In order for the django admin site media to work you
        # must change @DJANGO_ROOT@ to be the path to your django
        # installation, which is probably something like:
        # /usr/lib/python2.6/site-packages/django
        Alias /media/ "/usr/local/lib64/python2.6/site-packages/django/contrib/admin/media/"
        <Location "/media/">
                SetHandler None
        </Location>

        # The graphite.wsgi file has to be accessible by apache. It won't
        # be visible to clients because of the DocumentRoot though.
        <Directory /opt/graphite/conf/>
                Allow from all
        </Directory>

</VirtualHost>
```


### <a name="Optional">Optional</a> ###

```
  612  sudo chown -R wwwrun:wwwrun .
  613  sudo chgrp -R wwwrun .
  614  less /etc/group
  615  sudo chown -R wwwrun:www .

if working on VM add Guest Additions. Toolbar:
Devices -> Insert Guest Addition CD image

Error: Kernel sources missing
YaST:
kernel-source 3.0
kernel-default-devel
gcc
```


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_app.md" class="module">exometer_graphite_app</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_reporter.md" class="module">exometer_graphite_reporter</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_subscribers.md" class="module">exometer_graphite_subscribers</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_graphite/blob/master/doc/exometer_graphite_subscribers_sup.md" class="module">exometer_graphite_subscribers_sup</a></td></tr></table>

