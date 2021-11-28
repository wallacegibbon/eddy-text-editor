-module(eddy).

-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, _} = editEventManager:start_link(),
    {ok, _} = keyToWordService:start_link(),
    {ok, _} = inputStateMachine:start_link(),
    ok = editEventManager:add_handler(cursesWindowManager),
    ok.
