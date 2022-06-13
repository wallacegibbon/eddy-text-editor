-module(eddy_t9_translator).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, start_link/0, query/1, freq_count/1, stop/0]).
-behaviour(gen_server).

%%-type t9_key_stroke() :: $2..$9.
-type t9_key_stroke() :: integer().
-type word_freq_map() :: #{Word :: binary() => Frequency :: integer()}.
-type word_dictionary_item() :: {WordKeyStroke :: [t9_key_stroke()], WordString :: binary()}.
-type word_dict() :: [word_dictionary_item()].
-type state() :: #{word_dict => word_dict(), word_freq_map => word_freq_map(), freq_changed => boolean()}.

-define(WORDS_FILE, "./words.txt").
%%-define(FREQUENCY_FILE, "/usr/share/eddy/frequency.dat").
-define(FREQUENCY_FILE, "./frequency.dat").
-define(SERVER, ?MODULE).
-define(FREQ_MAP_SAVE_PERIOD, 10000).

handle_call({query, Keys}, _From, #{word_dict := WordDict, word_freq_map := FreqMap} = State) ->
    {reply, find_words(Keys, WordDict, FreqMap), State}.

handle_cast({use, Word}, #{word_freq_map := FreqMap} = State) ->
    NewFreqMap = maps:update_with(Word, fun (V) -> V + 1 end, 1, FreqMap),
    {noreply, State#{word_freq_map := NewFreqMap, freq_changed := true}}.

handle_info(save_freq_map, #{word_freq_map := FreqMap, freq_changed := true} = State) ->
    dump_frequency(FreqMap),
    erlang:send_after(?FREQ_MAP_SAVE_PERIOD, ?SERVER, save_freq_map),
    {noreply, State#{freq_changed := false}};
handle_info(save_freq_map, #{freq_changed := false} = State) ->
    erlang:send_after(?FREQ_MAP_SAVE_PERIOD, ?SERVER, save_freq_map),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #{word_freq_map := FreqMap}) ->
    dump_frequency(FreqMap).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:send_after(?FREQ_MAP_SAVE_PERIOD, ?SERVER, save_freq_map),
    {ok, WordRowsText} = file:read_file(?WORDS_FILE),
    WordDict = mk_word_dictionary(WordRowsText, []),
    {ok, #{word_dict => WordDict, word_freq_map => load_freq_map(), freq_changed => false}}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec query([char()]) -> [string()].
query(Keys) ->
    gen_server:call(?SERVER, {query, Keys}).

-spec freq_count(binary()) -> ok.
freq_count(Word) ->
    gen_server:cast(?SERVER, {use, Word}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% In the T9 input method:
%% 2 -> ABC, 3 -> DEF, 4 -> GHI, 5 -> JKL, 6 -> MNO, 7 -> PQRS, 8 -> TUV, 9 -> WXYZ
-spec chars_to_keys([char()]) -> [t9_key_stroke()].
chars_to_keys(CharList) ->
    lists:map(fun char_to_key_unify_case/1, CharList).

-spec char_to_key_unify_case(char()) -> t9_key_stroke().
char_to_key_unify_case(A) when A >= $A, A =< $Z ->
    char_to_key(A - $A + $a);
char_to_key_unify_case(A) when A >= $a, A =< $z ->
    char_to_key(A).

-spec char_to_key(integer()) -> t9_key_stroke().
char_to_key(Number) when Number < $s ->
    $2 + (Number - $a) div 3;
char_to_key($s) ->
    $7;
char_to_key(Number) when Number >= $t, Number < $v ->
    $8;
char_to_key(_) ->
    $9.

%% since there is no space inside words, all spaces can be simply ignored
-spec get_one_word(binary(), [char()]) -> {[char()], binary()} | nothing.
get_one_word(<<C, Rest/binary>>, Cs) when C >= $a, C =< $z ->
    get_one_word(Rest, [C | Cs]);
get_one_word(<<$\n, Rest/binary>>, Cs) ->
    {lists:reverse(Cs), Rest};
get_one_word(<<_, Rest/binary>>, Cs) ->
    get_one_word(Rest, Cs);
get_one_word(<<>>, _) ->
    nothing.

%% Non-ascii words will be ignored
-spec mk_word_dictionary(binary(), Result) -> Result when Result :: word_dict().
mk_word_dictionary(WordRows, Result) ->
    case get_one_word(WordRows, []) of
        {Cs, Rest} ->
            try chars_to_keys(Cs) of
                Keys ->
                    mk_word_dictionary(Rest, [{Keys, list_to_binary(Cs)} | Result])
            catch
                _:_ ->
                    mk_word_dictionary(Rest, Result)
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

-spec load_freq_map() -> word_freq_map().
load_freq_map() ->
    ensure_file_exist(?FREQUENCY_FILE, fun () -> dump_frequency(#{}) end),
    try file:consult(?FREQUENCY_FILE) of
        {ok, [FreqMap]} ->
            FreqMap
    catch
        _:_ ->
            #{}
    end.

-spec ensure_file_exist(string(), fun (() -> ok)) -> ok.
ensure_file_exist(FileName, AbsenceHandler) ->
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            AbsenceHandler()
    end.

-spec dump_frequency(word_freq_map()) -> ok.
dump_frequency(FreqMap) ->
    {ok, OutputFd} = file:open(?FREQUENCY_FILE, [write]),
    io:format(OutputFd, "~p.", [FreqMap]),
    ok = file:close(OutputFd).

-spec find_words([char()], word_dict(), word_freq_map()) -> [string()].
find_words([], _, _) ->
    [];
find_words(Keys, WordDict, FreqMap) ->
    CandidateList = [Word || {WordKeys, Word} <- WordDict, match_keys(Keys, WordKeys)],
    prepare_result(CandidateList, FreqMap).

-spec prepare_result([binary()], word_freq_map()) -> [string()].
prepare_result(WordCandidateList, FreqMap) ->
    R1 = lists:sort(fun (A, B) when byte_size(A) =/= byte_size(B) ->
                            byte_size(A) =< byte_size(B);
                        (_, _) ->
                            true
                    end,
                    WordCandidateList),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun (A, B) when byte_size(A) =:= byte_size(B) ->
                            compare_frequency(A, B, FreqMap);
                        (_, _) ->
                            true
                    end,
                    R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

-spec compare_frequency(binary(), binary(), word_freq_map()) -> boolean().
compare_frequency(Word1, Word2, FreqMap) ->
    maps:get(Word1, FreqMap, 0) > maps:get(Word2, FreqMap, 0).
