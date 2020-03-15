-module(eddy).

-export([start/0]).

-include_lib("cecho/include/cecho.hrl").

-include("eddy_keys.hrl").

start_keylistener() ->
    Pid = self(),
    spawn_link(fun() -> listen_key(m1, Pid, [], []) end).


listen_key(Mode, Parent, Keys, Options) ->
    case translate_char(cecho:getch()) of
	{ok, C} ->
	    handle_key(Mode, Parent, [C | Keys], Options);
	error ->
	    listen_key(Mode, Parent, Keys, Options)
    end.

handle_key(m1, Parent, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Parent ! {word_option, Options},
    listen_key(m1, Parent, Keys, Options);

handle_key(m1, Parent, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    Parent ! {word_option, Options},
    listen_key(m1, Parent, RKeys, Options);

handle_key(m1, Parent, [$0 | RKeys], Options) ->
    listen_key(m1, Parent, RKeys, Options);

%% when the word is selected, empty the word list and options
handle_key(m1, Parent, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Parent ! {word_insert, Word},
    listen_key(m1, Parent, [], []);

%% in any mode, a direct enter or space is self inserting
handle_key(Mode, Parent, [C], []) when C =:= $\s; C =:= $\n ->
    Parent ! {word_insert, [C]},
    listen_key(Mode, Parent, [], []);

handle_key(m1, Parent, [$\b, _ | Keys], _) ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Parent ! {word_option, Options},
    listen_key(m1, Parent, Keys, Options);

handle_key(Mode, Parent, [$\b], _) ->
    listen_key(Mode, Parent, [], []);

handle_key(m1, Parent, [?NEXTMAP | _], _) ->
    listen_key(m2, Parent, [], []);

handle_key(m2, Parent, [?NEXTMAP | _], _) ->
    listen_key(m1, Parent, [], []);

handle_key(m2, Parent, [C], []) when C >= $0, C =< $9 ->
    Parent ! {word_insert, [C]},
    listen_key(m2, Parent, [], []);

handle_key(m2, Parent, [$\b], []) ->
    Parent ! delete_char,
    listen_key(m2, Parent, [], []);

handle_key(Mode, Parent, Keys, Options) ->
    Parent ! {error, {Mode, Keys, Options}},
    error.


translate_char(C) ->
    Map = #{$w => $1, $e => $2, $r => $3, $s => $4, $d => $5, $f => $6,
	    $x => $7, $c => $8, $v => $9, $b => $0, $\s => $\s, $g => $\n,
	    $t => $\b, $2 => ?NEXTMAP, $3 => ?NEXTMODE, $4 => ?FN},
    maps:find(C, Map).


spacen(0) -> [];
spacen(N) -> [$\s | spacen(N - 1)].

clear_nextline() ->
    {_, MCol} = cecho:getmaxyx(),
    {Row, Col} = cecho:getyx(),
    cecho:move(Row + 1, 0),
    cecho:addstr(spacen(MCol)),
    cecho:move(Row + 2, 0),
    cecho:addstr(spacen(MCol)),
    cecho:move(Row, Col).

draw_options([Word | RestOptions]) ->
    {MRow, MCol} = cecho:getmaxyx(),
    {Row, Col} = cecho:getyx(),
    cecho:addstr(spacen(MCol - Col)),
    cecho:move(Row, Col),
    cecho:addstr(Word),
    clear_nextline(),
    cecho:move(Row + 1, Col),
    cecho:addstr(string:join(RestOptions, " ")),
    cecho:move(Row, Col).

main_loop() ->
    receive
	{word_option, Options} ->
	    %cecho:addstr(io_lib:format("<ops: ~w>", [Options])),
	    draw_options(Options),
	    cecho:refresh();
	{word_insert, Str} ->
	    %cecho:addstr(io_lib:format("<str: ~w>", [Str])),
	    cecho:addstr(Str),
	    clear_nextline(),
	    cecho:refresh();
	delete_char ->
	    todo;
	{error, I} ->
	    cecho:addstr(io_lib:format("<err: ~w>", [I])),
	    cecho:refresh()
    end,
    main_loop().


start() ->
    wordsvc:start_link(),
    application:start(cecho),
    ok = cecho:noecho(),
    start_keylistener(),
    main_loop().

