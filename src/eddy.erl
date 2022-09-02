-module(eddy).
-export([start/0]).

-spec start() -> no_return().
start() ->
	{ok, _} = eddy_t9_translator:start_link(),
	{ok, _} = eddy_state_machine:start_link(),
	{ok, _} = eddy_edit_event:start(),
	ok = eddy_edit_event:add_handler(eddy_terminal_ui),
	ok.

