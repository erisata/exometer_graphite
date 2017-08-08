

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_env-1">get_env/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#name-0">name/0</a></td><td></td></tr><tr><td valign="top"><a href="#priv_dir-0">priv_dir/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#version-0">version/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_env-1"></a>

### get_env/1 ###

<pre><code>
get_env(Name::atom()) -&gt; undefined | {ok, Value::term()}
</code></pre>
<br />

<a name="get_env-2"></a>

### get_env/2 ###

<pre><code>
get_env(Name::atom(), Default::term()) -&gt; Value::term()
</code></pre>
<br />

<a name="name-0"></a>

### name/0 ###

<pre><code>
name() -&gt; atom()
</code></pre>
<br />

<a name="priv_dir-0"></a>

### priv_dir/0 ###

`priv_dir() -> any()`

<a name="start-2"></a>

### start/2 ###

`start(StartType, StartArgs) -> any()`

<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

<a name="version-0"></a>

### version/0 ###

`version() -> any()`

