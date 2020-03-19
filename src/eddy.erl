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


start_keylistener() ->
    Pid = self(),
    spawn_link(fun() -> listen_key(normal, Pid, [], []) end).


listen_key(Mode, Pid, Keys, Arguments) ->
    case pretranslate(cecho:getch()) of
	{ok, C} ->
	    handle_key(Mode, Pid, [C | Keys], Arguments);
	error ->
	    listen_key(Mode, Pid, Keys, Arguments)
    end.

%% back to normal with double click on MAPCHANGE
handle_key(chmap, Pid, [chmap, chmap], _) ->
    Pid ! {word_option, []},
    listen_key(normal, Pid, [], []);

%% prepare map selection
handle_key(_, Pid, [chmap | _], _) ->
    listen_key(chmap, Pid, [chmap], []);

handle_key(chmap, Pid, [N, chmap], _) when N >= $1, N =< $9 ->
    Pid ! {word_option, []},
    listen_key({sym, N - $0}, Pid, [], []);

handle_key(chmap, Pid, [_, chmap], _) ->
    Pid ! {word_option, []},
    listen_key(normal, Pid, [], []);

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

%% T9 input method
handle_key(normal, Pid, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Pid ! {word_option, Options},
    listen_key(normal, Pid, Keys, Options);

handle_key(normal, Pid, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    Pid ! {word_option, Options},
    listen_key(normal, Pid, RKeys, Options);

handle_key(normal, Pid, [$1], []) ->
    listen_key(normal, Pid, [], []);

handle_key(normal, Pid, [$0 | RKeys], Options) ->
    listen_key(normal, Pid, RKeys, Options);

%% when the word is selected, empty the word list and options
handle_key(normal, Pid, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Pid ! {word_insert, Word},
    wordsvc:freqcount(Word),
    listen_key(normal, Pid, [], []);

%% in any mode, a direct enter or space is self inserting
handle_key(Mode, Pid, [C], []) when C =:= $\s; C =:= $\n ->
    Pid ! {word_insert, [C]},
    listen_key(Mode, Pid, [], []);

handle_key(normal, Pid, [$\b, _ | Keys], _) ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Pid ! {word_option, Options},
    listen_key(normal, Pid, Keys, Options);

handle_key(normal, Pid, [$\b], _) ->
    listen_key(normal, Pid, [], []);

handle_key(Mode, Pid, [$\b], []) when Mode =/= normal ->
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


translatecmd(Key1, Key2) -> maps:get([Key1, Key2], ?COMMANDS, unknown).

pretranslate(C) -> maps:find(char_to_lower(C), ?BASICMAP).

translatesym(Key, N) -> maps:get(Key, get_symmap(N), $\s).

get_symmap(1) -> ?SYM1;
get_symmap(2) -> ?SYM2;
get_symmap(3) -> ?SYM3;
get_symmap(4) -> ?SYM4;
get_symmap(5) -> ?SYM5;
get_symmap(_) -> #{}.


char_to_lower(C) when C >= $A, C =< $Z -> C + ($a - $A);
char_to_lower(C) -> C.

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

clear_rest_chars() ->
    {_, MCol} = cecho:getmaxyx(),
    {Row, Col} = cecho:getyx(),
    cecho:addstr(spacen(MCol - Col)),
    cecho:move(Row, Col).

draw_options([Word | RestOptions]) ->
    clear_rest_chars(),
    clear_nextline(),
    {Row, Col} = cecho:getyx(),
    cecho:addstr(Word),
    cecho:move(Row + 1, Col),
    cecho:addstr(string:join(RestOptions, " ")),
    cecho:move(Row, Col);

draw_options([]) ->
    clear_rest_chars(),
    clear_nextline().

main_loop() ->
    receive
	{cmd, Cmd} ->
	    cecho:addstr(io_lib:format("<cmd: ~w>", [Cmd])),
	    cecho:refresh(),
	    main_loop();
	{word_option, Options} ->
	    %cecho:addstr(io_lib:format("<ops: ~w>", [Options])),
	    draw_options(Options),
	    cecho:refresh(),
	    main_loop();
	{word_insert, Str} ->
	    %cecho:addstr(io_lib:format("<str: ~w>", [Str])),
	    cecho:addstr(Str),
	    clear_nextline(),
	    cecho:refresh(),
	    main_loop();
	delete_char ->
	    main_loop();
	{error, I} ->
	    cecho:addstr(io_lib:format("<err: ~w>", [I])),
	    cecho:refresh(),
	    main_loop();
	stop ->
	    wordsvc:stop(),
	    ok = application:stop(cecho),
	    stopped
    end.


start() ->
    wordsvc:start_link(),
    ok = application:start(cecho),
    ok = cecho:noecho(),
    start_keylistener(),
    main_loop().

