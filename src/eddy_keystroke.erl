-module(eddy_keystroke).
-export([pre_translate/1, translate_command/2, translate_key/2]).
-export_type([key/0]).

-type key()				:: integer() | change_map_key | change_mode_key | command_key.

-define(COMMANDS,
	#{
	"22" => complete, "11" => capitalize, "12" => upper, "13" => lower, "14" => join1, "16" => join2,
	"33" => undo, "32" => redo, "66" => find, "88" => select, "77" => cut, "78" => copy, "79" => paste,
	"44" => save, "55" => quit, "45" => save_and_quit
	}).

-define(BASEMAP,
	#{
	$w => $1, $e => $2, $r => $3, $s => $4, $d => $5, $f => $6, $x => $7, $c => $8, $v => $9, $b => $0,
	$\s => $\s, $g => $\n, $t => $\b, $3 => change_map_key, $2 => change_mode_key, $4 => command_key
	}).

-define(MAP1,
	#{
	$1 => $1, $2 => $2, $3 => $3, $4 => $4, $5 => $5, $6 => $6, $7 => $7, $8 => $8, $9 => $9, $0 => $0,
	$\n => $\n, $\b => $\b
	}).

-define(MAP2,
	#{
	$1 => $[, $3 => $], $4 => $(, $6 => $), $7 => ${, $9 => $}, $2 => $", $5 => $', $8 => $., $0 => $,,
	$\n => $\n, $\b => $\b
	}).

-define(MAP3,
	#{
	$1 => $\\, $2 => $|, $3 => $/, $4 => $<, $5 => $=, $6 => $>, $7 => $?, $8 => $#, $9 => $:, $0 => $;,
	$\n => $\n, $\b => $\b
	}).

-define(MAP4,
	#{
	$1 => $+, $2 => $*, $3 => $-, $4 => $@, $5 => $%, $6 => $&, $7 => $!, $8 => $~, $9 => $^, $0 => $_,
	$\n => $\n, $\b => $\b
	}).

-define(MAP5,
	#{
	$1 => $$, $2 => $`, $\n => $\n, $\b => $\b
	}).

-spec pre_translate(char()) -> {ok, key()} | error.
pre_translate(C) ->
	maps:find(lowercase(C), ?BASEMAP).

%% Commands in eddy are all 2-keystroke
-spec translate_command(char(), char()) -> atom().
translate_command(Key1, Key2) ->
	maps:get([Key1, Key2], ?COMMANDS, unknown).

-spec translate_key(char(), integer()) -> char().
translate_key(Key, KeyMapIndex) ->
	maps:get(Key, map_on_index(KeyMapIndex), $\s).

-spec map_on_index(integer()) -> #{char() => char()}.
map_on_index(1) -> ?MAP1;
map_on_index(2) -> ?MAP2;
map_on_index(3) -> ?MAP3;
map_on_index(4) -> ?MAP4;
map_on_index(5) -> ?MAP5;
map_on_index(_) -> #{}.

-spec lowercase(char()) -> char().
lowercase(C) when C >= $A, C =< $Z	-> C - $A + $a;
lowercase(C)				-> C.

