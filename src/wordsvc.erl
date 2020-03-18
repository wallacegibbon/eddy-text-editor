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

translate_by_offset(N) when N < 18 -> $2 + N div 3;
translate_by_offset(N) when N =:= 18 -> $7;
translate_by_offset(N) when N >= 19, N < 22 -> $8;
translate_by_offset(_) -> $9.

fetch_words(<<A:8/integer, R/binary>>, W) when A =/= $\n ->
    fetch_words(R, [A | W]);
fetch_words(<<_:8/integer, R/binary>>, W) ->
    {lists:reverse(W), R};
fetch_words(<<>>, _) ->
    nothing.

%% Non-ascii words will be ignored
translate_words(Ws, Result) ->
    case fetch_words(Ws, []) of
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

match_keys([C | R1], [C | R2]) -> match_keys(R1, R2);
match_keys([C1 | _], [C2 | _]) when C1 =/= C2 -> false;
match_keys([_ | _], []) -> false;
match_keys([], _) -> true.

load_words() ->
    {ok, Stream} = file:read_file(?WORDS_FILE),
    Dict = translate_words(Stream, []),
    {ok, [Frequency]} = file:consult(?FREQUENCY_FILE),
    {Dict, Frequency}.

dump_frequency(Frequency) ->
    {ok, F} = file:open(?FREQUENCY_FILE, write),
    io:format(F, "~p.", [Frequency]),
    ok = file:close(F).


find_words([_ | _] = Keys, Dict, Frequency) ->
    Ws = [Word || {KeyList, Word} <- Dict, match_keys(Keys, KeyList)],
    if Ws =/= [] ->
	   prepare_result(Ws, Frequency);
       true ->
	   [Keys]
    end;

find_words([], _, _) ->
    [].

prepare_result(Lst, Frequency) ->
    R1 = lists:sort(fun(A, B) when size(A) =/= size(B) -> size(A) =< size(B);
		       (_, _) -> true
		    end, Lst),
    R2 = lists:sublist(R1, 10),
    R3 = lists:sort(fun(A, B) when size(A) =:= size(B) ->
			    frequency_compare(A, B, Frequency);
		       (_, _) -> true
		    end, R2),
    R4 = lists:map(fun binary_to_list/1, R3),
    R4.

frequency_compare(Word1, Word2, Frequency) ->
    A = case maps:find(Word1, Frequency) of
	    {ok, Cnt1} -> Cnt1;
	    error -> 0
	end,
    B = case maps:find(Word2, Frequency) of
	    {ok, Cnt2} -> Cnt2;
	    error -> 0
	end,
    A > B.

search_loop({Dict, Frequency}) ->
    receive
	{query, Pid, Keys} ->
	    Pid ! {words, find_words(Keys, Dict, Frequency)},
	    search_loop({Dict, Frequency});

	{use, Word} ->
	    W = list_to_binary(Word),
	    case maps:find(W, Frequency) of
		{ok, Cnt} ->
		    search_loop({Dict, Frequency#{W := Cnt + 1}});
		error ->
		    search_loop({Dict, Frequency#{W => 1}})
	    end;

	stop ->
	    stopped

    after 20000 ->
	dump_frequency(Frequency),
	search_loop({Dict, Frequency})
    end.

start_link() ->
    register(wordsvc, spawn_link(fun() ->
					 search_loop(load_words())
				 end)).

query(Keys) ->
    wordsvc ! {query, self(), Keys},
    receive
	{words, Words} -> Words
    end.

freqcount(Word) ->
    wordsvc ! {use, Word}.

stop() ->
    wordsvc ! stop.


