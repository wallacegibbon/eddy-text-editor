-module(t9InputUtil).

-export([preTranslate/1, translateCommand/2, translateKey/2]).

-include("./t9InputUtil.hrl").

-define(COMMANDS, #{"22" => complete, "11" => capitalize, "12" => upper, "13" => lower, "14" => join1, "16" => join2, "33" => undo, "32" => redo,
                    "66" => find, "88" => select, "77" => cut, "78" => copy, "79" => paste, "44" => save, "55" => quit, "45" => saveAndQuit}).

-define(BASEMAP, #{$w => $1, $e => $2, $r => $3, $s => $4, $d => $5, $f => $6, $x => $7, $c => $8, $v => $9, $b => $0,
                   $\s => $\s, $g => $\n, $t => $\b, $3 => changeMapKey, $2 => changeModeKey, $4 => commandKey}).

-define(MAP1, #{$1 => $1, $2 => $2, $3 => $3, $4 => $4, $5 => $5, $6 => $6, $7 => $7, $8 => $8, $9 => $9, $0 => $0, $\n => $\n, $\b => $\b}).

-define(MAP2, #{$1 => $[, $3 => $], $4 => $(, $6 => $), $7 => ${, $9 => $}, $2 => $", $5 => $', $8 => $., $0 => $,, $\n => $\n, $\b => $\b}).

-define(MAP3, #{$1 => $\\, $2 => $|, $3 => $/, $4 => $<, $5 => $= , $6 => $>, $7 => $?, $8 => $#, $9 => $:, $0 => $;, $\n => $\n, $\b => $\b}).

-define(MAP4, #{$1 => $+, $2 => $*, $3 => $-, $4 => $@, $5 => $%, $6 => $&, $7 => $!, $8 => $~, $9 => $^, $0 => $_, $\n => $\n, $\b => $\b}).

-define(MAP5, #{$1 => $$, $2 => $`, $\n => $\n, $\b => $\b}).

-spec preTranslate(char()) -> {ok, baseKeyStroke()} | error.
preTranslate(C) ->
    maps:find(characterLowerCase(C), ?BASEMAP).

%% Commands in eddy are all 2-keystroke
-spec translateCommand(char(), char()) -> atom().
translateCommand(Key1, Key2) ->
    maps:get([Key1, Key2], ?COMMANDS, unknown).

-spec translateKey(char(), integer()) -> char().
translateKey(Key, KeyMapIndex) ->
    maps:get(Key, getMapOfIndex(KeyMapIndex), $\s).

-spec getMapOfIndex(integer()) -> #{char() => char()}.
getMapOfIndex(1) -> ?MAP1;
getMapOfIndex(2) -> ?MAP2;
getMapOfIndex(3) -> ?MAP3;
getMapOfIndex(4) -> ?MAP4;
getMapOfIndex(5) -> ?MAP5;
getMapOfIndex(_) -> #{}.

-spec characterLowerCase(char()) -> char().
characterLowerCase(C) when C >= $A, C =< $Z ->
    C - $A + $a;
characterLowerCase(C) ->
    C.
