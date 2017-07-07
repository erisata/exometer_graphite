

# The yaws_cage application #

This application provides various utility functions for
implementing services as [`http://yaws.hyber.org/`](http://yaws.hyber.org/) appmods.

The approach for implementing REST services is taken
from [`http://www.infoq.com/articles/vinoski-erlang-rest`](http://www.infoq.com/articles/vinoski-erlang-rest).
Example usage of [`yaws_cage_rest`](yaws_cage_rest.md) module:

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


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="yaws_cage_logger.md" class="module">yaws_cage_logger</a></td></tr>
<tr><td><a href="yaws_cage_rest.md" class="module">yaws_cage_rest</a></td></tr></table>

