-module(eddy).

-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, _} = keyToWordService:start_link(),
    {ok, _} = inputStateMachine:start_link(),
    {ok, _} = editEventManager:start(),
    ok = editEventManager:add_handler(cursesWindowManager),
    ok.
