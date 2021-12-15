-module(eddy).
-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, _} = eddy_word_translator:start_link(),
    {ok, _} = eddy_input_statem:start_link(),
    {ok, _} = eddy_edit_event:start(),
    ok = eddy_edit_event:add_handler(eddy_curses_window),
    ok.
