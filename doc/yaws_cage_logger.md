

# Module yaws_cage_logger #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Almost empty implementation of the yaws logger and crash handler.

__Behaviours:__ [`yaws_logger`](yaws_logger.md).

<a name="description"></a>

## Description ##
See `logger_mod` and `errormod_crash` options in the `yaws.config`.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close_log-3">close_log/3</a></td><td></td></tr><tr><td valign="top"><a href="#crashmsg-3">crashmsg/3</a></td><td></td></tr><tr><td valign="top"><a href="#open_log-3">open_log/3</a></td><td></td></tr><tr><td valign="top"><a href="#wrap_log-4">wrap_log/4</a></td><td></td></tr><tr><td valign="top"><a href="#write_log-4">write_log/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close_log-3"></a>

### close_log/3 ###

`close_log(ServerName, Type, State) -> any()`

<a name="crashmsg-3"></a>

### crashmsg/3 ###

`crashmsg(Arg, SC, L) -> any()`

<a name="open_log-3"></a>

### open_log/3 ###

`open_log(ServerName, Type, LogDir) -> any()`

<a name="wrap_log-4"></a>

### wrap_log/4 ###

`wrap_log(ServerName, Type, State, LogWrapSize) -> any()`

<a name="write_log-4"></a>

### write_log/4 ###

`write_log(ServerName, Type, State, Infos) -> any()`

