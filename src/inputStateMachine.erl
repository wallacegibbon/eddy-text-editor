-module(inputStateMachine).

-behaviour(gen_statem).

-export([start_link/0]).

-export([init/1, callback_mode/0, t9Insert/3, directInsert/3, waitCommand1/3, waitCommand2/3, changeMap/3]).

-include("./t9InputUtil.hrl").

-define(SERVER, ?MODULE).

-record(inputStateMachineState, {collectedKeys = [] :: [baseKeyStroke()], mapIndex = 0 :: integer(), previousMode :: eddyMode()}).

t9Insert(cast, changeMap, #inputStateMachineState{}) ->
    {next_state, changeMap, #inputStateMachineState{collectedKeys = [changeMap]}};
t9Insert(cast, Key, #inputStateMachineState{} = State) when Key =:= $\s; Key =:= $\n ->
    gen_server:cast(cursesWindowManager, {insertCharacter, [Key]}),
    {next_state, t9Insert, State};
t9Insert(cast, command, #inputStateMachineState{}) ->
    {next_state, waitCommand1, #inputStateMachineState{collectedKeys = [command], previousMode = t9Insert}};
t9Insert(cast, Character, State) ->
    {next_state, t9Insert, State}.

directInsert(cast, changeMap, #inputStateMachineState{} = State) ->
    {next_state, changeMap, State#inputStateMachineState{collectedKeys = [changeMap]}};
directInsert(cast, Key, #inputStateMachineState{} = State) when Key =:= $\s; Key =:= $\n ->
    gen_server:cast(cursesWindowManager, {insertCharacter, [Key]}),
    {next_state, directInsert, State};
directInsert(cast, command, #inputStateMachineState{}) ->
    {next_state, waitCommand1, #inputStateMachineState{collectedKeys = [command], previousMode = directInsert}};
directInsert(cast, Key, #inputStateMachineState{mapIndex = MapIndex} = State) ->
    gen_server:cast(cursesWindowManager, {insertCharacter, [t9InputUtil:translateKey(Key, MapIndex)]}),
    {next_state, directInsert, State}.

waitCommand1(cast, command, #inputStateMachineState{collectedKeys = [command]} = State) ->
    {next_state, waitCommand1, State};
waitCommand1(cast, Key, #inputStateMachineState{collectedKeys = [command]} = State) ->
    {next_state, waitCommand2, State#inputStateMachineState{collectedKeys = [Key, command]}}.

waitCommand2(cast, command, #inputStateMachineState{collectedKeys = [_, command]} = State) ->
    {next_state, waitCommand1, State#inputStateMachineState{collectedKeys = [command]}};
waitCommand2(cast, Key2, #inputStateMachineState{collectedKeys = [Key1, command], previousMode = PreviousMode} = State) ->
    gen_server:cast(cursesWindowManager, {command, t9InputUtil:translateCommand(Key1, Key2)}),
    {next_state, PreviousMode, State#inputStateMachineState{collectedKeys = []}}.

%% back to t9 insert mode with double click on MAP_CHANGE
changeMap(cast, changeMap, #inputStateMachineState{collectedKeys = [changeMap]} = State) ->
    {next_state, t9Insert, State#inputStateMachineState{collectedKeys = []}};
changeMap(cast, Key, #inputStateMachineState{collectedKeys = [changeMap]} = State) when Key >= $1, Key =< $9 ->
    gen_server:cast(cursesWindowManager, stopInput),
    {next_state, changeMap, State};
%% ignore invalid keys
changeMap(cast, _, #inputStateMachineState{collectedKeys = [changeMap]} = State) ->
    {next_state, changeMap, State};
changeMap(cast, Key, #inputStateMachineState{collectedKeys = [changeMap]}) when Key >= $1, Key =< $9 ->
    gen_server:cast(cursesWindowManager, stopInput),
    {next_state, directInsert, #inputStateMachineState{mapIndex = Key - $0}};
changeMap(cast, changeMap, #inputStateMachineState{collectedKeys = [_, changeMap]} = State) ->
    {next_state, changeMap, State#inputStateMachineState{collectedKeys = [changeMap]}}.


init([]) ->
    {ok, t9Insert, #inputStateMachineState{}}.

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
