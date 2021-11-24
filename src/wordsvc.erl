-module(wordsvc).

-export([frequencyCount/1, dumpFrequency/1, start_link/0, query/1, stop/0]).

-define(WORDS_FILE, "./words.txt").
%-define(FREQUENCY_FILE, "/usr/share/eddy/frequency.dat").
-define(FREQUENCY_FILE, "./frequency.dat").

%-type t9KeyStroke() :: $2..$9.
-type t9KeyStroke() :: integer().
-type wordFrequencyMap() :: #{binary() => integer()}.
-type wordDictionaryItem() :: {[t9KeyStroke()], binary()}.
-type wordDictionary() :: [wordDictionaryItem()].

%% In the T9 input method, 2 <-> abc, 3 <-> def, 4 <-> ghi, 5 <-> jkl, 6 <-> mno, 7 <-> pqrs, 8 <-> tuv, 9 <-> wxyz
-spec characterListToKeyList([char()]) -> [t9KeyStroke()].
characterListToKeyList(AlphabetList) ->
    lists:map(fun characterToKey/1, AlphabetList).

-spec characterToKey(char()) -> t9KeyStroke().
characterToKey(A) when A >= $A, A =< $Z ->
    characterOffsetToKey(A - $A);
characterToKey(A) when A >= $a, A =< $z ->
    characterOffsetToKey(A - $a).

-spec characterOffsetToKey(integer()) -> t9KeyStroke().
characterOffsetToKey(Number) when Number < 18 ->
    $2 + Number div 3;
characterOffsetToKey(Number) when Number =:= 18 ->
    $7;
characterOffsetToKey(Number) when Number >= 19, Number < 22 ->
    $8;
characterOffsetToKey(_) ->
    $9.

%% since there is no space inside words, all spaces can be simply ignored
-spec fetchWordFromLine(binary(), [char()]) -> {[char()], binary()} | nothing.
fetchWordFromLine(<<Alphabet:8/integer, Rest/binary>>, CollectedAlphabets) when Alphabet >= $a, Alphabet =< $z ->
    fetchWordFromLine(Rest, [Alphabet | CollectedAlphabets]);
fetchWordFromLine(<<Alphabet:8/integer, Rest/binary>>, CollectedAlphabets) when Alphabet =:= $\n ->
    {lists:reverse(CollectedAlphabets), Rest};
fetchWordFromLine(<<_:8/integer, Rest/binary>>, CollectedAlphabets) ->
    fetchWordFromLine(Rest, CollectedAlphabets);
fetchWordFromLine(<<>>, _) ->
    nothing.

%% Non-ascii words will be ignored
-spec buildWordDictionary(binary(), Result) -> Result when Result :: wordDictionary().
buildWordDictionary(WordLinesText, Result) ->
    case fetchWordFromLine(WordLinesText, []) of
        {AlphabetList, RestBinary} ->
            try characterListToKeyList(AlphabetList) of
                KeyList ->
                    buildWordDictionary(RestBinary, [{KeyList, list_to_binary(AlphabetList)} | Result])
            catch
                _:_ ->
                    buildWordDictionary(RestBinary, Result)
            end;
        nothing ->
            Result
    end.

-spec matchKeys([t9KeyStroke()], [t9KeyStroke()]) -> boolean().
matchKeys([Key | RestKeys1], [Key | RestKeys2]) ->
    matchKeys(RestKeys1, RestKeys2);
matchKeys([Key1 | _], [Key2 | _]) when Key1 =/= Key2 ->
    false;
matchKeys([_ | _], []) ->
    false;
matchKeys([], _) ->
    true.

-spec loadWordDictionary() -> {wordDictionary(), wordFrequencyMap()}.
loadWordDictionary() ->
    {ok, WordRowsText} = file:read_file(?WORDS_FILE),
    WordDictionary = buildWordDictionary(WordRowsText, []),
    {WordDictionary, loadWordFrequency()}.

-spec loadWordFrequency() -> wordFrequencyMap().
loadWordFrequency() ->
    ensureFileExist(?FREQUENCY_FILE, fun () -> dumpFrequency(#{}) end),
    try file:consult(?FREQUENCY_FILE) of
        {ok, [FrequencyMap]} ->
            FrequencyMap
    catch
        _:_ ->
            #{}
    end.

-spec ensureFileExist(string(), fun (() -> ok)) -> ok.
ensureFileExist(FileName, HandleWhenNotExist) ->
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            HandleWhenNotExist()
    end.

-spec dumpFrequency(wordFrequencyMap()) -> ok.
dumpFrequency(FrequencyMap) ->
    {ok, OutputFileHandler} = file:open(?FREQUENCY_FILE, [write]),
    io:format(OutputFileHandler, "~p.", [FrequencyMap]),
    ok = file:close(OutputFileHandler).

-spec findWords([char()], wordDictionary(), wordFrequencyMap()) -> [string()].
findWords(Keys, WordDictionary, FrequencyMap) when Keys =/= [] ->
    WordResultList = [Word || {KeyList, Word} <- WordDictionary, matchKeys(Keys, KeyList)],
    prepareResult(WordResultList, FrequencyMap);
findWords([], _, _) ->
    [].

-spec prepareResult([binary()], wordFrequencyMap()) -> [string()].
prepareResult(WordResultList, FrequencyMap) ->
    R1 = lists:sort(fun(A, B) when size(A) =/= size(B) -> size(A) =< size(B);
                       (_, _) -> true
                    end, WordResultList),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun(A, B) when size(A) =:= size(B) -> compareFrequency(A, B, FrequencyMap);
                       (_, _) -> true
                    end, R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

-spec compareFrequency(binary(), binary(), wordFrequencyMap()) -> boolean().
compareFrequency(Word1, Word2, FrequencyMap) ->
    maps:get(Word1, FrequencyMap, 0) > maps:get(Word2, FrequencyMap, 0).

-type searchLoopState() :: {wordDictionary(), wordFrequencyMap()}.

-spec searchLoop(searchLoopState()) -> no_return().
searchLoop({WordDictionary, FrequencyMap} = State) ->
    receive
        {{Pid, Ref}, Command} ->
            case handleSearchCommand(Command, State) of
                {reply, Result, NewState} ->
                    Pid ! {Ref, Result},
                    searchLoop(NewState);
                {stop, _} ->
                    dumpFrequency(FrequencyMap),
                    Pid ! {Ref, stopped}
            end;
        {use, Word} ->
            NewFrequency = maps:update_with(Word, fun(V) -> V + 1 end, 1, FrequencyMap),
            searchLoop({WordDictionary, NewFrequency})
    end.

-spec handleSearchCommand(any(), searchLoopState()) -> {reply, any(), searchLoopState()} | {stop, stopped}.
handleSearchCommand({query, Keys}, {WordDictionary, FrequencyMap} = State) ->
    Result = findWords(Keys, WordDictionary, FrequencyMap),
    {reply, Result, State};
handleSearchCommand(stop, _) ->
    {stop, stopped}.

-spec start_link() -> true.
start_link() ->
    register(?MODULE, spawn_link(fun() -> searchLoop(loadWordDictionary()) end)).

-spec callCommand(any()) -> any().
callCommand(Command) ->
    Ref = erlang:make_ref(),
    ?MODULE ! {{self(), Ref}, Command},
    receive
        {Ref, Result} ->
            Result
    end.

-spec query([char()]) -> [string()].
query(Keys) ->
    callCommand({query, Keys}).

-spec stop() -> stopped.
stop() ->
    callCommand(stop).

-spec frequencyCount(binary()) -> any().
frequencyCount(Word) ->
    ?MODULE ! {use, Word}.
