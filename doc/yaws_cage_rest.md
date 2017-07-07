

# Module yaws_cage_rest #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

YAWS Appmod for basic REST services.

__This module defines the `yaws_cage_rest` behaviour.__<br /> Required callback functions: `handle_request/4`.

<a name="description"></a>

## Description ##

You can use this module as follows:

```
   -module(my_rest).
   -behaviour(yaws_cage_rest).
   -export([out/1, handle_request/4]).
   -include_lib("yaws/include/yaws_api.hrl").
   out(Arg) ->
       yaws_cage_rest:out(?MODULE, Arg, _Opts = []).
   handle_request(["check"], 'GET', _Arg, _Opts) ->
       {content, 200, <<"ok!">>};
   handle_request(Path, _Method, Arg, Opts) ->
       yaws_cage_rest:handle_unsupported(?MODULE, Path, Arg, Opts).
```

See [`http://www.infoq.com/articles/vinoski-erlang-rest`](http://www.infoq.com/articles/vinoski-erlang-rest).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#appmod_path-1">appmod_path/1</a></td><td>
Returns a path, at which the appmod is mounted.</td></tr><tr><td valign="top"><a href="#handle_unsupported-4">handle_unsupported/4</a></td><td>
Default fallback for unsupported cases.</td></tr><tr><td valign="top"><a href="#out-3">out/3</a></td><td>
This function should be called from the yaws appmod, the Module:out/1 function.</td></tr><tr><td valign="top"><a href="#serve_file-4">serve_file/4</a></td><td>
Serves a specified file from the file system.</td></tr><tr><td valign="top"><a href="#serve_priv_file-4">serve_priv_file/4</a></td><td>
Serves a file from th application's priv directory.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="appmod_path-1"></a>

### appmod_path/1 ###

`appmod_path(Arg) -> any()`

Returns a path, at which the appmod is mounted.
The path is always returned with / at the end.

`<AppmodPath>/<AppmodData> = <PrePath><Appmod>/AppmodData = <ServerPath>`

<a name="handle_unsupported-4"></a>

### handle_unsupported/4 ###

`handle_unsupported(Module, Path, Arg, Opts) -> any()`

Default fallback for unsupported cases.

<a name="out-3"></a>

### out/3 ###

<pre><code>
out(Module::module(), Arg::#arg{}, Opts::#{debug =&gt; boolean(), aggregate_chunks =&gt; boolean()}) -&gt; YawsResponse::term()
</code></pre>
<br />

This function should be called from the yaws appmod, the Module:out/1 function.
It will delegate the calls to the Module:handle_request/4 function.

This function recognizes the following options:

* `debug` -- if set to true, will log request info.

* `aggregate_chunks` -- can be set to false, in order
to disable automatic aggregation of chunked requests.

<a name="serve_file-4"></a>

### serve_file/4 ###

`serve_file(Module, Path, Arg, Opts) -> any()`

Serves a specified file from the file system.

Module is used here only for error reporting.

Path is a relative or absolute path. It can be either
string or a list of path elements.

Opts can have the following:

* `content_type` -- the content type of the response.
It is determined automatically, if not provided.

* `variables` -- if non-empty map is provided, it is used
to perform replacements in the served file (trivial
template engine).

<a name="serve_priv_file-4"></a>

### serve_priv_file/4 ###

`serve_priv_file(Module, Path, Arg, Opts) -> any()`

Serves a file from th application's priv directory.
The application is determined from the Module.

__See also:__ [serve_file/4](#serve_file-4).

