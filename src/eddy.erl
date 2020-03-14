-module(eddy).

-export([start/0]).

-include_lib("cecho/include/cecho.hrl").
-define(SPACEKEY, 32).

start() ->
    wordsvc:start_link(),
    application:start(cecho),
    ok = cecho:noecho(),
    %cecho:move(1, 1),
    {Row, Col} = cecho:getyx(),
    {MRow, MCol} = cecho:getmaxyx(),
    read_chars(),
    %application:stop(cecho),
    ok.

read_chars() ->
    case cecho:getch() of 
	?SPACEKEY ->
	    cecho:addch(?SPACEKEY),
	    cecho:refresh();
	C ->
	    try_getword([C])
    end,
    read_chars().

try_getword(CList) ->
    case cecho:getch() of
	?SPACEKEY ->
	    case wordsvc:query(lists:reverse(translate_charlst(CList))) of
		[First | _] ->
		    cecho:addstr(binary_to_list(First));
		[] ->
		    cecho:addstr(io_lib:format("~w", [CList]))
	    end,
	    cecho:refresh();
	C ->
	    try_getword([C | CList])
    end.

translate_charlst([C | Rst]) ->
    case translate_char(C) of
	{ok, V} ->
	    [V | translate_charlst(Rst)];
	_ ->
	    translate_charlst(Rst)
    end;
translate_charlst([]) ->
    [].

translate_char(C) ->
    KeyMap = #{$w => 1, $e => 2, $r => 3, $s => 4, $d => 5, $f => 6,
	       $x => 7, $c => 8, $v => 9},
    maps:find(C, KeyMap).

