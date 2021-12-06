-module(keyToWordService).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, start_link/0, query/1, frequencyCount/1, stop/0]).

-define(SERVER, ?MODULE).
-define(SAVE_FREQUENCY_MAP_PERIOD, 10000).

%-type t9KeyStroke() :: $2..$9.
-type t9KeyStroke() :: integer().
-type wordFrequencyMap() :: #{binary() => integer()}.
-type wordDictionaryItem() :: {[t9KeyStroke()], binary()}.
-type wordDictionary() :: [wordDictionaryItem()].

-type keyToWordsServiceState() :: #{wordDictionary => wordDictionary(), wordFrequencyMap => wordFrequencyMap(), wordFrequencyChanged => boolean()}.

-define(WORDS_FILE, "./words.txt").
%-define(FREQUENCY_FILE, "/usr/share/eddy/frequency.dat").
-define(FREQUENCY_FILE, "./frequency.dat").

handle_call({query, Keys}, _From, #{wordDictionary := WordDictionary, wordFrequencyMap := FrequencyMap} = State) ->
    {reply, findWords(Keys, WordDictionary, FrequencyMap), State}.

handle_cast({use, Word}, #{wordFrequencyMap := FrequencyMap} = State) ->
    NewFrequencyMap = maps:update_with(Word, fun (V) -> V + 1 end, 1, FrequencyMap),
    {noreply, State#{wordFrequencyMap := NewFrequencyMap, wordFrequencyChanged := true}}.

handle_info(saveFrequencyMap, #{wordFrequencyMap := FrequencyMap, wordFrequencyChanged := true} = State) ->
    dumpFrequency(FrequencyMap),
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, saveFrequencyMap),
    {noreply, State#{wordFrequencyChanged := false}};
handle_info(saveFrequencyMap, #{wordFrequencyChanged := false} = State) ->
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, saveFrequencyMap),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #{wordFrequencyMap := FrequencyMap}) ->
    dumpFrequency(FrequencyMap).

-spec init([]) -> {ok, keyToWordsServiceState()}.
init([]) ->
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, saveFrequencyMap),
    {ok, WordRowsText} = file:read_file(?WORDS_FILE),
    WordDictionary = buildWordDictionary(WordRowsText, []),
    {ok, #{wordDictionary => WordDictionary, wordFrequencyMap => loadWordFrequency(), wordFrequencyChanged => false}}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec query([char()]) -> [string()].
query(Keys) ->
    gen_server:call(?SERVER, {query, Keys}).

-spec frequencyCount(binary()) -> ok.
frequencyCount(Word) ->
    gen_server:cast(?SERVER, {use, Word}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

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
    R1 = lists:sort(fun (A, B) when size(A) =/= size(B) -> size(A) =< size(B);
                        (_, _) -> true
                    end, WordResultList),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun (A, B) when size(A) =:= size(B) -> compareFrequency(A, B, FrequencyMap);
                        (_, _) -> true
                    end, R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

-spec compareFrequency(binary(), binary(), wordFrequencyMap()) -> boolean().
compareFrequency(Word1, Word2, FrequencyMap) ->
    maps:get(Word1, FrequencyMap, 0) > maps:get(Word2, FrequencyMap, 0).
