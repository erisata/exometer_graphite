

# Module exometer_graphite_subscribers #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Responsible for managing subscription configuration given in `sys.config`.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#force_resubscribe-0">force_resubscribe/0</a></td><td>
Used to start resubscription manually.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td>
Updates subscriptions to metrics and continues message sending loop.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>
Sets up subscription configuration loop.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Start a subscription manager.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td>
Unused.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

Unused.

<a name="force_resubscribe-0"></a>

### force_resubscribe/0 ###

`force_resubscribe() -> any()`

Used to start resubscription manually.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Unknown, From, State) -> any()`

Unused.

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Unknown, State) -> any()`

Unused.

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Unknown, State) -> any()`

Updates subscriptions to metrics and continues message sending loop.

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

Sets up subscription configuration loop.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start a subscription manager.

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

Unused.

