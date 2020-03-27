-module(wordsvc).

-export([start_link/0, query/1, freqcount/1, stop/0]).

-define(WORDS_FILE, "./words.txt").
-define(FREQUENCY_FILE, "/usr/share/eddy/frequency.dat").

translate_word([A | Rest]) ->
    [translate_alpha(A) | translate_word(Rest)];
translate_word([]) ->
    [].

translate_alpha(A) when A >= $A, A =< $Z ->
    translate_by_offset(A - $A);
translate_alpha(A) when A >= $a, A =< $z ->
    translate_by_offset(A - $a).

translate_by_offset(N) when N < 18 ->
    $2 + N div 3;
translate_by_offset(N) when N =:= 18 ->
    $7;
translate_by_offset(N) when N >= 19, N < 22 ->
    $8;
translate_by_offset(_) ->
    $9.

%% since there is no space inside words, all spaces can be simply ignored
fetch_word(<<A:8/integer, R/binary>>, W) when A >= $a, A =< $z ->
    fetch_word(R, [A | W]);
fetch_word(<<A:8/integer, R/binary>>, W) when A =:= $\n ->
    {lists:reverse(W), R};
fetch_word(<<_:8/integer, R/binary>>, W) ->
    fetch_word(R, W);
fetch_word(<<>>, _) ->
    nothing.

%% Non-ascii words will be ignored
translate_words(Ws, Result) ->
    case fetch_word(Ws, []) of
	{W, RestBinary} ->
	    try translate_word(W) of
		Ww ->
		    translate_words(RestBinary,
				    [{Ww, list_to_binary(W)} | Result])
	    catch
		_:_ ->
		    translate_words(RestBinary, Result)
	    end;
	nothing ->
	    Result
    end.

match_keys([C | R1], [C | R2]) ->
    match_keys(R1, R2);
match_keys([C1 | _], [C2 | _]) when C1 =/= C2 ->
    false;
match_keys([_ | _], []) ->
    false;
match_keys([], _) ->
    true.

load_words() ->
    {ok, Stream} = file:read_file(?WORDS_FILE),
    Dict = translate_words(Stream, []),
    {ok, [FreqMap]} = file:consult(?FREQUENCY_FILE),
    {Dict, FreqMap}.

dump_frequency(FreqMap) ->
    {ok, F} = file:open(?FREQUENCY_FILE, write),
    io:format(F, "~p.", [FreqMap]),
    ok = file:close(F).


find_words(Keys, Dict, FreqMap) when Keys =/= [] ->
    Ws = [Word || {KeyList, Word} <- Dict, match_keys(Keys, KeyList)],
   prepare_result(Ws, FreqMap);
find_words([], _, _) ->
    [].

prepare_result(Lst, FreqMap) ->
    R1 = lists:sort(fun(A, B) when size(A) =/= size(B) -> size(A) =< size(B);
		       (_, _) -> true
		    end, Lst),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun(A, B) when size(A) =:= size(B) ->
			    freqcmp(A, B, FreqMap);
		       (_, _) -> true
		    end, R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

freqcmp(Word1, Word2, FreqMap) ->
    maps:get(Word1, FreqMap, 0) > maps:get(Word2, FreqMap, 0).


search_loop({Dict, FreqMap}) ->
    receive
	{query, Pid, Keys} ->
	    Pid ! {words, find_words(Keys, Dict, FreqMap)},
	    search_loop({Dict, FreqMap});
	{use, Word} ->
	    NewFreq = maps:update_with(list_to_binary(Word),
				       fun(V) -> V + 1 end, 1, FreqMap),
	    search_loop({Dict, NewFreq});
	{stop, Pid} ->
	    dump_frequency(FreqMap),
	    Pid ! {wordsvc, stopped}
    end.

start_link() ->
    register(wordsvc, spawn_link(fun() ->
					 search_loop(load_words())
				 end)).

query(Keys) ->
    wordsvc ! {query, self(), Keys},
    receive
	{words, Words} ->
	    Words
    end.

freqcount(Word) ->
    wordsvc ! {use, Word}.

stop() ->
    wordsvc ! {stop, self()},
    receive
	{wordsvc, stopped} ->
	    stopped
    end.

