#! /usr/bin/env escript
%%! -noinput -pa ../eddy/_build/default/lib/cecho/ebin ../eddy/_build/default/lib/eddy/ebin

-mode(compile).

main(_) ->
    eddy:start().

