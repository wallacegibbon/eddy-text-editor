-module(eddy).

-export([start/0]).

-include_lib("cecho/include/cecho.hrl").

-include("eddy_keys.hrl").

start_keylistener() ->
    Pid = self(),
    spawn_link(fun() -> listen_key(normal, Pid, [], []) end).


listen_key(Mode, Parent, Keys, Options) ->
    case translatekey_pre(cecho:getch()) of
	{ok, C} ->
	    handle_key(Mode, Parent, [C | Keys], Options);
	error ->
	    listen_key(Mode, Parent, Keys, Options)
    end.

%% back to normal with double click on MAPCHANGE
handle_key(chmap, Parent, [?MAPCHANGE, ?MAPCHANGE], _) ->
    Parent ! {word_option, []},
    listen_key(normal, Parent, [], []);

%% prepare map selection
handle_key(_, Parent, [?MAPCHANGE | _], _) ->
    listen_key(chmap, Parent, [?MAPCHANGE], []);

handle_key(chmap, Parent, [N, ?MAPCHANGE], _) when N >= $1, N =< $9 ->
    Parent ! {word_option, []},
    listen_key({sym, N - $0}, Parent, [], []);

handle_key(chmap, Parent, [_, ?MAPCHANGE], _) ->
    Parent ! {word_option, []},
    listen_key(normal, Parent, [], []);

%% command mode
handle_key(waitcmd1, Parent, [?FN, ?FN], OldMode) ->
    listen_key(waitcmd1, Parent, [?FN], OldMode);

handle_key(Mode, Parent, [?FN | _], _) ->
    listen_key(waitcmd1, Parent, [?FN], Mode);

handle_key(waitcmd1, Parent, [A, ?FN], OldMode) ->
    listen_key(waitcmd2, Parent, [A, ?FN], OldMode);

handle_key(waitcmd2, Parent, [B, A, ?FN], OldMode) ->
    case translate_command(A, B) of
	Quit when Quit =:= quit; Quit =:= savequit ->
	    Parent ! stop;
	Cmd ->
	    Parent ! {cmd, Cmd},
	    listen_key(OldMode, Parent, [], [])
    end;

%% T9 input method
handle_key(normal, Parent, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Parent ! {word_option, Options},
    listen_key(normal, Parent, Keys, Options);

handle_key(normal, Parent, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    Parent ! {word_option, Options},
    listen_key(normal, Parent, RKeys, Options);

handle_key(normal, Parent, [$1], []) ->
    listen_key(normal, Parent, [], []);

handle_key(normal, Parent, [$0 | RKeys], Options) ->
    listen_key(normal, Parent, RKeys, Options);

%% when the word is selected, empty the word list and options
handle_key(normal, Parent, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Parent ! {word_insert, Word},
    wordsvc:freqcount(Word),
    listen_key(normal, Parent, [], []);

%% in any mode, a direct enter or space is self inserting
handle_key(Mode, Parent, [C], []) when C =:= $\s; C =:= $\n ->
    Parent ! {word_insert, [C]},
    listen_key(Mode, Parent, [], []);

handle_key(normal, Parent, [$\b, _ | Keys], _) ->
    Options = wordsvc:query(lists:reverse(Keys)),
    Parent ! {word_option, Options},
    listen_key(normal, Parent, Keys, Options);

handle_key(normal, Parent, [$\b], _) ->
    listen_key(normal, Parent, [], []);

handle_key(Mode, Parent, [$\b], []) when Mode =/= normal ->
    Parent ! delete_char,
    listen_key(Mode, Parent, [], []);

%% direct key maps
handle_key({sym, N} = Mode, Parent, [C], []) ->
    Parent ! {word_insert, [maps:get(C, get_symmap(N), $\s)]},
    listen_key(Mode, Parent, [], []);

%% there should not be any situation left
handle_key(Mode, Parent, Keys, Options) ->
    Parent ! {error, {Mode, Keys, Options}},
    listen_key(Mode, Parent, [], []).


translate_command(Key1, Key2) ->
    Cmd = #{"22" => complete, "11" => capitalize, "12" => upper, "13" => lower,
	    "14" => join1, "16" => join2,
	    "33" => undo, "32" => redo,
	    "66" => find, "88" => select,
	    "77" => cut, "78" => copy, "79" => paste,
	    "44" => save, "55" => quit, "45" => savequit},
    maps:get([Key1, Key2], Cmd, unknown).

translatekey_pre(C) ->
    Pre = #{$w => $1, $e => $2, $r => $3, $s => $4, $d => $5, $f => $6,
	    $x => $7, $c => $8, $v => $9, $b => $0, $\s => $\s, $g => $\n,
	    $t => $\b, $3 => ?MAPCHANGE, $2 => ?MODECHANGE, $4 => ?FN},
    maps:find(char_to_lower(C), Pre).

get_symmap(1) ->
    #{$1 => $1, $2 => $2, $3 => $3, $4 => $4, $5 => $5, $6 => $6,
      $7 => $7, $8 => $8, $9 => $9, $0 => $0};

get_symmap(2) ->
    #{$1 => $[, $3 => $], $4 => $(, $6 => $), $7 => ${, $9 => $},
      $2 => $", $5 => $', $8 => $., $0 => $,};

get_symmap(3) ->
    #{$1 => $\\, $2 => $|, $3 => $/, $4 => $<, $5 => $=, $6 => $>,
      $7 => $^, $8 => $!, $9 => $:, $0 => $;};

get_symmap(4) ->
    #{$1 => $+, $2 => $-, $3 => $*, $4 => $@, $5 => $_, $6 => $#,
      $7 => $~, $8 => $%, $9 => $$, $0 => $&};

get_symmap(5) ->
    #{$1 => $?, $2 => $`};

get_symmap(_) ->
    #{}.


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

