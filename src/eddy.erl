-module(eddy).
-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, _} = key_to_word_service:start_link(),
    {ok, _} = input_state_machine:start_link(),
    {ok, _} = edit_event_manager:start(),
    ok = edit_event_manager:add_handler(curses_window_manager),
    ok.
