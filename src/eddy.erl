-module(eddy).

-export([start/0]).

-define(COMMANDS, #{"22" => complete, "11" => capitalize, "12" => upper,
		    "13" => lower, "14" => join1, "16" => join2,
		    "33" => undo, "32" => redo, "66" => find, "88" => select,
		    "77" => cut, "78" => copy, "79" => paste,
		    "44" => save, "55" => quit, "45" => savequit}).

-define(BASICMAP, #{$w => $1, $e => $2, $r => $3, $s => $4, $d => $5,
		    $f => $6, $x => $7, $c => $8, $v => $9, $b => $0,
		    $\s => $\s, $g => $\n, $t => $\b,
		    $3 => chmap, $2 => chmod, $4 => fn}).

-define(SYM1, #{$1 => $1, $2 => $2, $3 => $3, $4 => $4, $5 => $5,
		$6 => $6, $7 => $7, $8 => $8, $9 => $9, $0 => $0}).

-define(SYM2, #{$1 => $[, $3 => $], $4 => $(, $6 => $), $7 => ${,
		$9 => $}, $2 => $", $5 => $', $8 => $., $0 => $,}).

-define(SYM3, #{$1 => $\\, $2 => $|, $3 => $/, $4 => $<, $5 => $=,
		$6 => $>, $7 => $^, $8 => $!, $9 => $:, $0 => $;}).

-define(SYM4, #{$1 => $+, $2 => $-, $3 => $*, $4 => $@, $5 => $_,
		$6 => $#, $7 => $~, $8 => $%, $9 => $$, $0 => $&}).

-define(SYM5, #{$1 => $?, $2 => $`}).

-define(T9WINROWS, 5).
-define(T9WINCOLS, 20).


start_keylistener() ->
    Pid = self(),
    spawn_link(fun() -> listen_key(t9_start, Pid, [], []) end).

listen_key(Mode, Pid, Keys, Arguments) ->
    case pretranslate(cecho:getch()) of
	{ok, C} ->
	    handle_key(Mode, Pid, [C | Keys], Arguments);
	error ->
	    listen_key(Mode, Pid, Keys, Arguments)
    end.

%% back to t9 with double click on MAPCHANGE
handle_key(chmap, Pid, [chmap, chmap], _) ->
    listen_key(t9_start, Pid, [], []);

%% prepare map selection
handle_key(_, Pid, [chmap | _], _) ->
    listen_key(chmap, Pid, [chmap], []);

handle_key(chmap, Pid, [N, chmap], _) when N >= $1, N =< $9 ->
    Pid ! t9_stop,
    listen_key({sym, N - $0}, Pid, [], []);

handle_key(chmap, Pid, [_, chmap], _) ->
    listen_key(chmap, Pid, [chmap], []);

%% command mode
handle_key(waitcmd1, Pid, [fn, fn], OldMode) ->
    listen_key(waitcmd1, Pid, [fn], OldMode);

handle_key(Mode, Pid, [fn | _], _) ->
    listen_key(waitcmd1, Pid, [fn], Mode);

handle_key(waitcmd1, Pid, [A, fn], OldMode) ->
    listen_key(waitcmd2, Pid, [A, fn], OldMode);

%% this is where this process may exit
handle_key(waitcmd2, Pid, [B, A, fn], OldMode) ->
    case translatecmd(A, B) of
	Quit when Quit =:= quit; Quit =:= savequit ->
	    Pid ! stop;
	Cmd ->
	    Pid ! {cmd, Cmd},
	    listen_key(OldMode, Pid, [], [])
    end;

%% in any input mode, a direct enter or space is self inserting
handle_key(Mode, Pid, [C], []) when C =:= $\s; C =:= $\n ->
    Pid ! {word_insert, [C]},
    listen_key(Mode, Pid, [], []);

%% T9 input method
handle_key(t9_start, Pid, [C] = Keys, _) when C >= $2, C =< $9 ->
    Pid ! t9_start,
    Options = wordsvc:query(Keys),
    Pid ! {word_option, Options},
    listen_key(t9, Pid, Keys, Options);

handle_key(t9_start, Pid, [_], _) ->
    listen_key(t9_start, Pid, [], []);

handle_key(t9, Pid, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Pid ! {word_option, Options},
    listen_key(t9, Pid, Keys, Options);

handle_key(t9, Pid, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    Pid ! {word_option, Options},
    listen_key(t9, Pid, RKeys, Options);

handle_key(t9, Pid, [$1], []) ->
    listen_key(t9, Pid, [], []);

handle_key(t9, Pid, [$0 | RKeys], Options) ->
    listen_key(t9, Pid, RKeys, Options);

%% when the word is selected, empty the word list and options
handle_key(t9, Pid, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Pid ! t9_stop,
    Pid ! {word_insert, Word},
    wordsvc:freqcount(Word),
    listen_key(t9_start, Pid, [], []);

handle_key(t9, Pid, [$\b, _ | Keys], _) when Keys =/= [] ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Pid ! {word_option, Options},
    listen_key(t9, Pid, Keys, Options);

handle_key(t9, Pid, [$\b, _], _) ->
    Pid ! t9_stop,
    listen_key(t9_start, Pid, [], []);

handle_key(t9, Pid, [$\b], _) ->
    listen_key(t9_start, Pid, [], []);

handle_key(Mode, Pid, [$\b], []) when Mode =/= t9 ->
    Pid ! delete_char,
    listen_key(Mode, Pid, [], []);

%% direct key maps
handle_key({sym, N} = Mode, Pid, [C], []) ->
    Pid ! {word_insert, [translatesym(C, N)]},
    listen_key(Mode, Pid, [], []);

%% there should not be any situation left
handle_key(Mode, Pid, Keys, Options) ->
    Pid ! {error, {Mode, Keys, Options}},
    listen_key(Mode, Pid, [], []).


pretranslate(C) -> maps:find(char_to_lower(C), ?BASICMAP).

translatecmd(Key1, Key2) -> maps:get([Key1, Key2], ?COMMANDS, unknown).

translatesym(Key, N) -> maps:get(Key, get_symmap(N), $\s).

get_symmap(1) -> ?SYM1;
get_symmap(2) -> ?SYM2;
get_symmap(3) -> ?SYM3;
get_symmap(4) -> ?SYM4;
get_symmap(5) -> ?SYM5;
get_symmap(_) -> #{}.

char_to_lower(C) when C >= $A, C =< $Z -> C + ($a - $A);
char_to_lower(C) -> C.


new_optionwin() ->
    {Row, Col} = cecho:getyx(),
    cecho:curs_set(0),
    W = cecho:newwin(?T9WINROWS, ?T9WINCOLS, Row+1, Col),
    cecho:wborder(W, $|, $|, $-, $-, $+, $+, $+, $+),
    W.

del_optionwin(T9Win) ->
    cecho:delwin(T9Win),
    cecho:curs_set(1),
    ok.

draw_options(#{t9window := T9Win} = Params, Options) ->
    {Row, Col} = cecho:getyx(),
    cecho:werase(T9Win),
    draw_option1(Params, lists:sublist(Options, ?T9WINROWS)),
    cecho:move(Row, Col).

draw_option1(#{t9window := T9Win} = Params, [Word | RestOptions]) ->
    {Row, _} = cecho:getyx(T9Win),
    cecho:waddstr(T9Win, Word),
    cecho:wmove(T9Win, Row+1, 0),
    draw_option1(Params, RestOptions);
draw_option1(#{t9window := T9Win}, []) ->
    cecho:wmove(T9Win, 0, 0),
    cecho:wrefresh(T9Win).

show_msg(Msg) ->
    {MRow, _} = cecho:getmaxyx(),
    {Row, Col} = cecho:getyx(),
    cecho:mvaddstr(MRow-1, 0, Msg),
    cecho:move(Row, Col),
    cecho:refresh().

stop_t9(#{t9window := T9Win}) -> del_optionwin(T9Win);
stop_t9(_) -> ok.

main_loop(Params) ->
    receive
	{cmd, Cmd} ->
	    show_msg(io_lib:format("command: ~w", [Cmd])),
	    main_loop(Params);
	t9_start ->
	    main_loop(Params#{t9window => new_optionwin()});
	t9_stop ->
	    stop_t9(Params),
	    main_loop(#{});
	{word_option, Options} ->
	    draw_options(Params, Options),
	    main_loop(Params);
	{word_insert, Str} ->
	    cecho:addstr(Str),
	    cecho:refresh(),
	    main_loop(Params);
	delete_char ->
	    %% todo
	    main_loop(Params);
	{error, I} ->
	    show_msg(io_lib:format("error: ~w", [I])),
	    main_loop(Params);
	stop ->
	    wordsvc:stop(),
	    ok = application:stop(cecho),
	    stopped
    end.

char_repeat(_, 0) -> [];
char_repeat(C, N) -> [C, char_repeat(C, N-1)].

fill_rows(R, R, _, _) ->
    ok;
fill_rows(R, MRow, MCol, Char) ->
    cecho:mvaddstr(R, 0, char_repeat(Char, MCol)),
    fill_rows(R+1, MRow, MCol, Char).

fill_screen(C) ->
    {MRow, MCol} = cecho:getmaxyx(),
    fill_rows(0, MRow, MCol, C),
    cecho:move(0, 0),
    cecho:refresh().

start() ->
    wordsvc:start_link(),
    ok = application:start(cecho),
    ok = cecho:noecho(),
    fill_screen($.),
    start_keylistener(),
    main_loop(#{}).

