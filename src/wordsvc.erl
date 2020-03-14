-module(wordsvc).

-export([start_link/0, query/1, stop/0]).

-define(WORDS_FILE, "./words.txt").

translate_word([A | Rest]) ->
    [translate_alpha(A) | translate_word(Rest)];
translate_word([]) ->
    [].

translate_alpha(A) when A >= $A, A =< $Z ->
    translate_by_offset(A - $A);
translate_alpha(A) when A >= $a, A =< $z ->
    translate_by_offset(A - $a).

translate_by_offset(N) when N < 18 -> N div 3 + 2;
translate_by_offset(N) when N =:= 18 -> 7;
translate_by_offset(N) when N >= 19, N < 22 -> 8;
translate_by_offset(_) -> 9.

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
    translate_words(Stream, []).

find_words(Keys, Dict) ->
    Ws = [Word || {KeyList, Word} <- Dict, match_keys(Keys, KeyList)],
    lists:sort(fun(A, B) -> size(A) =< size(B) end, Ws).

search_loop(Dict) ->
    receive
	{query, Pid, KeyList} ->
	    Pid ! {words, find_words(KeyList, Dict)},
	    search_loop(Dict);
	stop ->
	    stopped
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

stop() ->
    wordsvc ! stop.


