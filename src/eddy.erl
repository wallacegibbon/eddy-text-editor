-module(eddy).

-export([start/0]).

%% command mode

%% in any input mode, a direct enter or space is self inserting
%% T9 input method
handleKeyStroke(t9Start, Pid, [C] = Keys, _) when C >= $2, C =< $9 ->
    Pid ! startInput,
    Options = keyToWordService:query(Keys),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9Start, Pid, [_], _) ->
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = keyToWordService:query(lists:reverse(Keys)),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9, Pid, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    syncWordOptions(Pid, Options, RKeys),
    listenKeyStroke(t9, Pid, RKeys, Options);
handleKeyStroke(t9, Pid, [$1], []) ->
    listenKeyStroke(t9, Pid, [], []);
handleKeyStroke(t9, Pid, [$0 | RKeys], Options) ->
    listenKeyStroke(t9, Pid, RKeys, Options);
%% when the word is selected, empty the word list and options
handleKeyStroke(t9, Pid, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Pid ! stopInput,
    Pid ! {insertCharacter, Word},
    keyToWordService:frequencyCount(list_to_binary(Word)),
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [$\b, _], _) ->
    Pid ! stopInput,
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [$\b, _ | Keys], _) ->
    Options = keyToWordService:query(lists:reverse(Keys)),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9, Pid, [$\b], _) ->
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(Mode, Pid, [$\b], []) when Mode =/= t9 ->
    Pid ! deleteCharacter,
    listenKeyStroke(Mode, Pid, [], []);
%% there should not be any situation left
handleKeyStroke(Mode, Pid, Keys, Options) ->
    Pid ! {error, {Mode, Keys, Options}},
    listenKeyStroke(Mode, Pid, [], []).

-spec syncWordOptions(pid(), [string()], [baseKeyStroke()]) -> ok.
syncWordOptions(Pid, WordOptionList, Keys) ->
    Pid ! {wordsOptions, {WordOptionList, lists:reverse(Keys)}},
    ok.

-spec start() -> no_return().
start() ->
    {ok, _} = keyToWordService:start_link(),
    {ok, _} = cursesWindowManager:start_link(),
    {ok, _} = commandListener:start_link(),
    ok.
