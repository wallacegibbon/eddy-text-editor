-module(inputStateMachine).

-behaviour(gen_statem).

-export([init/1, callback_mode/0, handle_event/4, start_link/0]).

-include("./t9InputUtil.hrl").

-define(SERVER, ?MODULE).

-type keysAndWordOptions() :: {Keys :: [baseKeyStroke()], Options :: [string()]}.
-type commandModeData() :: {Keys :: [baseKeyStroke()], PreviousState :: eddyMode(), PreviousData :: stateData()}.
-type mapIndex() :: integer().

-type stateData() :: keysAndWordOptions() | mapIndex() | commandModeData().

-spec handle_event(cast, baseKeyStroke(), eddyMode(), stateData()) -> {next_state, eddyMode(), stateData()}.
handle_event(cast, commandKey, State, Data) when State =/= waitCommand1, State =/= waitCommand2 ->
    {next_state, waitCommand1, {[command], State, Data}};
%% When in command state (waitCommand1 or waitCommand2), clicking on the command key again will reset the state to waitCommand1
handle_event(cast, commandKey, _, {_, PreviousState, PreviousData}) ->
    {next_state, waitCommand1, {[], PreviousState, PreviousData}};
handle_event(cast, Key, waitCommand1, {_, PreviousState, PreviousData}) ->
    {next_state, waitCommand2, {[Key], PreviousState, PreviousData}};
handle_event(cast, Key2, waitCommand2, {[Key1], PreviousState, PreviousData}) ->
    gen_server:cast(cursesWindowManager, {command, t9InputUtil:translateCommand(Key1, Key2)}),
    {next_state, PreviousState, PreviousData};

%% Go back to t9 insert mode by double click on <MAP CHANGE> key.
handle_event(cast, changeMapKey, changeMap, _) ->
    gen_server:cast(cursesWindowManager, stopInput),
    {next_state, t9Start, {[], []}};
handle_event(cast, changeMapKey, _, Data) ->
    {next_state, changeMap, Data};
handle_event(cast, Key, changeMap, _) when Key >= $1, Key =< $9 ->
    gen_server:cast(cursesWindowManager, stopInput),
    {next_state, directInsert, Key - $0};
handle_event(cast, _, changeMap, Data) ->
    {next_state, changeMap, Data};

%% In direct-insert mode, the data is just an integer (key map index)
handle_event(cast, Key, directInsert, MapIndex) ->
    gen_server:cast(cursesWindowManager, {insertString, [t9InputUtil:translateKey(Key, MapIndex)]}),
    {next_state, directInsert, MapIndex};

%% The t9 input method logic
handle_event(cast, Key, t9Start, _) when Key >= $2, Key =< $9 ->
    gen_server:cast(cursesWindowManager, startInput),
    Options = keyToWordService:query([Key]),
    syncWordOptions(Options, [Key]),
    {next_state, t9Insert, {[Key], Options}};
handle_event(cast, $\b, t9Start, Data) ->
    {next_state, t9Start, Data};
handle_event(cast, Key, t9Insert, {CollectedKeys, _}) when Key >= $2, Key =< $9 ->
    Keys = [Key | CollectedKeys],
    Options = keyToWordService:query(lists:reverse(Keys)),
    syncWordOptions(Options, Keys),
    {next_state, t9Insert, {Keys, Options}};
handle_event(cast, $1, t9Insert, {Keys, [W | RestWords]}) ->
    Options = RestWords ++ [W],
    syncWordOptions(Options, Keys),
    {next_state, t9Insert, {Keys, Options}};
handle_event(cast, $1, t9Insert, {[], []} = Data) ->
    {next_state, t9Insert, Data};
%% when the word is selected, empty the word list and options
handle_event(cast, Key, t9Insert, {_, [Word | _]}) when Key =:= $\s; Key =:= $\n ->
    gen_server:cast(cursesWindowManager, stopInput),
    gen_server:cast(cursesWindowManager, {insertString, Word}),
    keyToWordService:frequencyCount(list_to_binary(Word)),
    {next_state, t9Start, {[], []}};
handle_event(cast, $\b, t9Insert, {[], _}) ->
    {next_state, t9Start, {[], []}};
handle_event(cast, $\b, State, Data) when State =/= t9Insert, State =/= t9Start ->
    gen_server:cast(cursesWindowManager, deleteCharacter),
    {next_state, State, Data};
handle_event(cast, $\b, t9Insert, {[_], _}) ->
    gen_server:cast(cursesWindowManager, stopInput),
    {next_state, t9Start, {[], []}};
handle_event(cast, $\b, t9Insert, {[_ | Keys], _}) ->
    Options = keyToWordService:query(lists:reverse(Keys)),
    syncWordOptions(Options, Keys),
    {next_state, t9Insert, {Keys, Options}};

%% In other cases, space and newline key stands for themselves
handle_event(cast, Key, State, Data) when Key =:= $\s; Key =:= $\n ->
    gen_server:cast(cursesWindowManager, {insertString, [Key]}),
    {next_state, State, Data};

%% There should not be other situations.
handle_event(cast, Key, State, Data) ->
    gen_server:cast(cursesWindowManager, {error, {Key, State, Data}}),
    {next_state, t9Start, {[], []}}.

-spec syncWordOptions([string()], [baseKeyStroke()]) -> ok.
syncWordOptions(WordOptionList, Keys) ->
    gen_server:cast(cursesWindowManager, {wordsOptions, {WordOptionList, lists:reverse(Keys)}}).

-spec init([]) -> {ok, eddyMode(), stateData()}.
init([]) ->
    {ok, t9Start, {[], []}}.

callback_mode() ->
    handle_event_function.

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
