

# Module exometer_graphite_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Entry point of the application.

__Behaviours:__ [`application`](application.md).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_env-1">get_env/1</a></td><td>
Get environment variable for this application.</td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td>
Get environment variable for this application.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>
Start the application.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Stop the application.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_env-1"></a>

### get_env/1 ###

<pre><code>
get_env(Name::atom()) -&gt; undefined | {ok, Value::term()}
</code></pre>
<br />

Get environment variable for this application.

<a name="get_env-2"></a>

### get_env/2 ###

<pre><code>
get_env(Name::atom(), Default::term()) -&gt; Value::term()
</code></pre>
<br />

Get environment variable for this application.

<a name="start-2"></a>

### start/2 ###

`start(StartType, StartArgs) -> any()`

Start the application.

<a name="stop-1"></a>

### stop/1 ###

`stop(State) -> any()`

Stop the application.

