-module(eddy_word_translator).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, start_link/0, query/1, frequency_count/1, stop/0]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SAVE_FREQUENCY_MAP_PERIOD, 10000).

%-type t9_key_stroke() :: $2..$9.
-type t9_key_stroke() :: integer().
-type word_frequency_map() :: #{Word :: binary() => Frequency :: integer()}.
-type word_dictionary_item() :: {WordKeyStroke :: [t9_key_stroke()], WordString :: binary()}.
-type word_dictionary() :: [word_dictionary_item()].

-type state() :: #{word_dictionary => word_dictionary(), word_frequency_map => word_frequency_map(), word_frequency_changed => boolean()}.

-define(WORDS_FILE, "./words.txt").
%-define(FREQUENCY_FILE, "/usr/share/eddy/frequency.dat").
-define(FREQUENCY_FILE, "./frequency.dat").

handle_call({query, Keys}, _From, #{word_dictionary := WordDictionary, word_frequency_map := FrequencyMap} = State) ->
    {reply, find_words(Keys, WordDictionary, FrequencyMap), State}.

handle_cast({use, Word}, #{word_frequency_map := FrequencyMap} = State) ->
    NewFrequencyMap = maps:update_with(Word, fun (V) -> V + 1 end, 1, FrequencyMap),
    {noreply, State#{word_frequency_map := NewFrequencyMap, word_frequency_changed := true}}.

handle_info(save_frequency_map, #{word_frequency_map := FrequencyMap, word_frequency_changed := true} = State) ->
    dump_frequency(FrequencyMap),
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, save_frequency_map),
    {noreply, State#{word_frequency_changed := false}};
handle_info(save_frequency_map, #{word_frequency_changed := false} = State) ->
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, save_frequency_map),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #{word_frequency_map := FrequencyMap}) ->
    dump_frequency(FrequencyMap).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:send_after(?SAVE_FREQUENCY_MAP_PERIOD, ?SERVER, save_frequency_map),
    {ok, WordRowsText} = file:read_file(?WORDS_FILE),
    WordDictionary = mk_word_dictionary(WordRowsText, []),
    {ok, #{word_dictionary => WordDictionary, word_frequency_map => load_word_frequency(), word_frequency_changed => false}}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec query([char()]) -> [string()].
query(Keys) ->
    gen_server:call(?SERVER, {query, Keys}).

-spec frequency_count(binary()) -> ok.
frequency_count(Word) ->
    gen_server:cast(?SERVER, {use, Word}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% In the T9 input method, 2 <-> abc, 3 <-> def, 4 <-> ghi, 5 <-> jkl, 6 <-> mno, 7 <-> pqrs, 8 <-> tuv, 9 <-> wxyz
-spec chars_to_keys([char()]) -> [t9_key_stroke()].
chars_to_keys(AlphabetList) ->
    lists:map(fun char_to_key/1, AlphabetList).

-spec char_to_key(char()) -> t9_key_stroke().
char_to_key(A) when A >= $A, A =< $Z ->
    char_offset_to_key(A - $A);
char_to_key(A) when A >= $a, A =< $z ->
    char_offset_to_key(A - $a).

-spec char_offset_to_key(integer()) -> t9_key_stroke().
char_offset_to_key(Number) when Number < 18 ->
    $2 + Number div 3;
char_offset_to_key(Number) when Number =:= 18 ->
    $7;
char_offset_to_key(Number) when Number >= 19, Number < 22 ->
    $8;
char_offset_to_key(_) ->
    $9.

%% since there is no space inside words, all spaces can be simply ignored
-spec word_from_line(binary(), [char()]) -> {[char()], binary()} | nothing.
word_from_line(<<Alphabet:8/integer, Rest/binary>>, CollectedAlphabets) when Alphabet >= $a, Alphabet =< $z ->
    word_from_line(Rest, [Alphabet | CollectedAlphabets]);
word_from_line(<<Alphabet:8/integer, Rest/binary>>, CollectedAlphabets) when Alphabet =:= $\n ->
    {lists:reverse(CollectedAlphabets), Rest};
word_from_line(<<_:8/integer, Rest/binary>>, CollectedAlphabets) ->
    word_from_line(Rest, CollectedAlphabets);
word_from_line(<<>>, _) ->
    nothing.

%% Non-ascii words will be ignored
-spec mk_word_dictionary(binary(), Result) -> Result when Result :: word_dictionary().
mk_word_dictionary(WordLinesText, Result) ->
    case word_from_line(WordLinesText, []) of
        {AlphabetList, RestBinary} ->
            try chars_to_keys(AlphabetList) of
                KeyList ->
                    mk_word_dictionary(RestBinary, [{KeyList, list_to_binary(AlphabetList)} | Result])
            catch
                _:_ ->
                    mk_word_dictionary(RestBinary, Result)
            end;
        nothing ->
            Result
    end.

-spec match_keys([t9_key_stroke()], [t9_key_stroke()]) -> boolean().
match_keys([Key | RestKeys1], [Key | RestKeys2]) ->
    match_keys(RestKeys1, RestKeys2);
match_keys([Key1 | _], [Key2 | _]) when Key1 =/= Key2 ->
    false;
match_keys([_ | _], []) ->
    false;
match_keys([], _) ->
    true.

-spec load_word_frequency() -> word_frequency_map().
load_word_frequency() ->
    ensure_file_exist(?FREQUENCY_FILE, fun () -> dump_frequency(#{}) end),
    try file:consult(?FREQUENCY_FILE) of
        {ok, [FrequencyMap]} ->
            FrequencyMap
    catch
        _:_ ->
            #{}
    end.

-spec ensure_file_exist(string(), fun (() -> ok)) -> ok.
ensure_file_exist(FileName, HandleWhenNotExist) ->
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            HandleWhenNotExist()
    end.

-spec dump_frequency(word_frequency_map()) -> ok.
dump_frequency(FrequencyMap) ->
    {ok, OutputFileHandler} = file:open(?FREQUENCY_FILE, [write]),
    io:format(OutputFileHandler, "~p.", [FrequencyMap]),
    ok = file:close(OutputFileHandler).

-spec find_words([char()], word_dictionary(), word_frequency_map()) -> [string()].
find_words(Keys, WordDictionary, FrequencyMap) when Keys =/= [] ->
    WordResultList = [Word || {KeyList, Word} <- WordDictionary, match_keys(Keys, KeyList)],
    prepare_result(WordResultList, FrequencyMap);
find_words([], _, _) ->
    [].

-spec prepare_result([binary()], word_frequency_map()) -> [string()].
prepare_result(WordResultList, FrequencyMap) ->
    R1 = lists:sort(fun (A, B) when size(A) =/= size(B) -> size(A) =< size(B);
                        (_, _) -> true
                    end, WordResultList),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun (A, B) when size(A) =:= size(B) -> compare_frequency(A, B, FrequencyMap);
                        (_, _) -> true
                    end, R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

-spec compare_frequency(binary(), binary(), word_frequency_map()) -> boolean().
compare_frequency(Word1, Word2, FrequencyMap) ->
    maps:get(Word1, FrequencyMap, 0) > maps:get(Word2, FrequencyMap, 0).
