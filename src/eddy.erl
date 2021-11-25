-module(eddy).

-export([start/0]).

-spec start() -> no_return().
start() ->
    {ok, _} = keyToWordService:start_link(),
    {ok, _} = cursesWindowManager:start_link(),
    {ok, _} = inputStateMachine:start_link(),
    ok.
