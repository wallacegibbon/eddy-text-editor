-module(eddy).

-export([start/0]).

-define(COMMANDS, #{"22" => complete, "11" => capitalize, "12" => upper, "13" => lower, "14" => join1, "16" => join2,
                    "33" => undo, "32" => redo, "66" => find, "88" => select, "77" => cut, "78" => copy, "79" => paste,
                    "44" => save, "55" => quit, "45" => saveAndQuit}).

-define(BASEMAP, #{$w => $1, $e => $2, $r => $3, $s => $4, $d => $5, $f => $6, $x => $7, $c => $8, $v => $9, $b => $0,
                   $\s => $\s, $g => $\n, $t => $\b, $3 => changeMap, $2 => changeMode, $4 => command}).

-define(MAP1, #{$1 => $1, $2 => $2, $3 => $3, $4 => $4, $5 => $5,
                $6 => $6, $7 => $7, $8 => $8, $9 => $9, $0 => $0}).

-define(MAP2, #{$1 => $[, $3 => $], $4 => $(, $6 => $), $7 => ${,
                $9 => $}, $2 => $", $5 => $', $8 => $., $0 => $, }).

-define(MAP3, #{$1 => $\\, $2 => $|, $3 => $/, $4 => $<, $5 => $= ,
                $6 => $>, $7 => $?, $8 => $#, $9 => $:, $0 => $;}).

-define(MAP4, #{$1 => $+, $2 => $*, $3 => $-, $4 => $@, $5 => $%,
                $6 => $&, $7 => $!, $8 => $~, $9 => $^, $0 => $_}).

-define(MAP5, #{$1 => $$, $2 => $`}).

-define(T9_WINDOW_ROWS, 5).
-define(T9_WINDOW_COLUMNS, 22).

-type eddyMode() :: t9Start | t9 | changeMap | waitCommand1 | waitCommand2 | {keyCollect, char()}.
-type baseKeyStroke() :: integer() | changeMap | changeMode | command.

%% start the key listening loop as a process.
-spec startKeyStrokeListener() -> pid().
startKeyStrokeListener() ->
    Pid = self(),
    spawn_link(fun() -> listenKeyStroke(t9Start, Pid, [], []) end).

-spec listenKeyStroke(eddyMode(), pid(), [baseKeyStroke()], any()) -> no_return().
listenKeyStroke(Mode, Pid, Keys, Arguments) ->
    try preTranslate(cecho:getch()) of
        {ok, Character} ->
            handleKeyStroke(Mode, Pid, [Character | Keys], Arguments);
        error ->
            listenKeyStroke(Mode, Pid, Keys, Arguments)
    catch
        exit:_ ->
            ok
    end.

%% back to t9 with double click on MAP_CHANGE
-spec handleKeyStroke(eddyMode(), pid(), [baseKeyStroke()], any()) -> no_return().
handleKeyStroke(changeMap, Pid, [changeMap, changeMap], _) ->
    listenKeyStroke(t9Start, Pid, [], []);
%% prepare map selection
handleKeyStroke(_, Pid, [changeMap | _], _) ->
    listenKeyStroke(changeMap, Pid, [changeMap], []);
handleKeyStroke(changeMap, Pid, [Key, changeMap], _) when Key >= $1, Key =< $9 ->
    Pid ! stopInput,
    listenKeyStroke({keyCollect, Key - $0}, Pid, [], []);
handleKeyStroke(changeMap, Pid, [_, changeMap], _) ->
    listenKeyStroke(changeMap, Pid, [changeMap], []);
%% command mode
handleKeyStroke(waitCommand1, Pid, [command, command], OldMode) ->
    listenKeyStroke(waitCommand1, Pid, [command], OldMode);
handleKeyStroke(Mode, Pid, [command | _], _) ->
    listenKeyStroke(waitCommand1, Pid, [command], Mode);
handleKeyStroke(waitCommand1, Pid, [A, command], OldMode) ->
    listenKeyStroke(waitCommand2, Pid, [A, command], OldMode);
%% this is where this process may exit
handleKeyStroke(waitCommand2, Pid, [B, A, command], OldMode) ->
    Pid ! {command, translateCommand(A, B)},
    listenKeyStroke(OldMode, Pid, [], []);
%% in any input mode, a direct enter or space is self inserting
handleKeyStroke(Mode, Pid, [C], []) when C =:= $\s; C =:= $\n ->
    Pid ! {insertCharacter, [C]},
    listenKeyStroke(Mode, Pid, [], []);
%% T9 input method
handleKeyStroke(t9Start, Pid, [C] = Keys, _) when C >= $2, C =< $9 ->
    Pid ! startInput,
    Options = wordsvc:query(Keys),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9Start, Pid, [_], _) ->
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [C | _] = Keys, _) when C >= $2, C =< $9 ->
    Options = wordsvc:query(lists:reverse(Keys)),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9, Pid, [$1 | RKeys], [W | Rest]) ->
    Options = Rest ++ [W],
    syncWordOptions(Pid, Options, RKeys),
    listenKeyStroke(t9, Pid, RKeys, Options);
handleKeyStroke(t9, Pid, [$1], []) ->
    listenKeyStroke(t9, Pid, [], []);
handleKeyStroke(t9, Pid, [$0 | RKeys], Options) ->
    listenKeyStroke(t9, Pid, RKeys, Options);
%% when the word is selected, empty the word list and options
handleKeyStroke(t9, Pid, [C | _], [Word | _]) when C =:= $\s; C =:= $\n ->
    Pid ! stopInput,
    Pid ! {insertCharacter, Word},
    wordsvc:frequencyCount(list_to_binary(Word)),
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [$\b, _], _) ->
    Pid ! stopInput,
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(t9, Pid, [$\b, _ | Keys], _) ->
    Options = wordsvc:query(lists:reverse(Keys)),
    syncWordOptions(Pid, Options, Keys),
    listenKeyStroke(t9, Pid, Keys, Options);
handleKeyStroke(t9, Pid, [$\b], _) ->
    listenKeyStroke(t9Start, Pid, [], []);
handleKeyStroke(Mode, Pid, [$\b], []) when Mode =/= t9 ->
    Pid ! deleteCharacter,
    listenKeyStroke(Mode, Pid, [], []);
%% direct key maps
handleKeyStroke({keyCollect, N} = Mode, Pid, [C], []) ->
    Pid ! {insertCharacter, [translateMap(C, N)]},
    listenKeyStroke(Mode, Pid, [], []);
%% there should not be any situation left
handleKeyStroke(Mode, Pid, Keys, Options) ->
    Pid ! {error, {Mode, Keys, Options}},
    listenKeyStroke(Mode, Pid, [], []).

-spec syncWordOptions(pid(), [string()], [baseKeyStroke()]) -> ok.
syncWordOptions(Pid, WordOptionList, Keys) ->
    Pid ! {wordsOptions, {WordOptionList, lists:reverse(Keys)}},
    ok.

-spec characterLowerCase(char()) -> char().
characterLowerCase(C) when C >= $A, C =< $Z ->
    C - $A + $a;
characterLowerCase(C) ->
    C.

-spec preTranslate(char()) -> {ok, baseKeyStroke()} | error.
preTranslate(C) ->
    maps:find(characterLowerCase(C), ?BASEMAP).

-spec translateCommand(char(), char()) -> atom().
translateCommand(Key1, Key2) ->
    maps:get([Key1, Key2], ?COMMANDS, unknown).

-spec translateMap(char(), integer()) -> char().
translateMap(Key, N) ->
    maps:get(Key, getMapOfIndex(N), $\s).

-spec getMapOfIndex(integer()) -> #{char() => char()}.
getMapOfIndex(1) -> ?MAP1;
getMapOfIndex(2) -> ?MAP2;
getMapOfIndex(3) -> ?MAP3;
getMapOfIndex(4) -> ?MAP4;
getMapOfIndex(5) -> ?MAP5;
getMapOfIndex(_) -> #{}.

-type cursesWindow() :: any().
-type mainLoopState() :: #{t9window => cursesWindow()}.

-spec newOptionWindow() -> cursesWindow().
newOptionWindow() ->
    {Row, Col} = cecho:getyx(),
    cecho:curs_set(0),
    W = cecho:newwin(?T9_WINDOW_ROWS, ?T9_WINDOW_COLUMNS, Row+1, Col),
    cecho:wborder(W, $|, $|, $-, $-, $+, $+, $+, $+),
    W.

-spec deleteOptionWindow(cursesWindow()) -> ok.
deleteOptionWindow(T9Win) ->
    cecho:delwin(T9Win),
    cecho:curs_set(1),
    ok.

-spec drawWordOptions([string()], mainLoopState()) -> ok.
drawWordOptions(Options, #{t9window := T9Win} = State) ->
    {Row, Col} = cecho:getyx(),
    cecho:werase(T9Win),
    drawWordOptionList(lists:sublist(Options, ?T9_WINDOW_ROWS), State),
    cecho:move(Row, Col),
    ok.

-spec drawWordOptionList([string()], mainLoopState()) -> ok.
drawWordOptionList([Word | RestOptions], #{t9window := T9Win} = State) ->
    {Row, _} = cecho:getyx(T9Win),
    cecho:waddstr(T9Win, Word),
    cecho:wmove(T9Win, Row+1, 0),
    drawWordOptionList(RestOptions, State);
drawWordOptionList([], #{t9window := T9Win}) ->
    cecho:wmove(T9Win, 0, 0),
    cecho:wrefresh(T9Win),
    ok.

-spec showMessage(any()) -> ok.
showMessage(Msg) ->
    {MRow, _} = cecho:getmaxyx(),
    {Row, Col} = cecho:getyx(),
    cecho:mvaddstr(MRow-1, 0, Msg),
    cecho:move(Row, Col),
    cecho:refresh(),
    ok.

-spec mainHandler(any(), mainLoopState()) -> {ok, mainLoopState()} | stopped.
mainHandler({command, Quit}, _) when Quit =:= quit; Quit =:= saveAndQuit ->
    cleanup(),
    stopped;
mainHandler({command, Cmd}, State) ->
    showMessage(io_lib:format("command: ~w", [Cmd])),
    {ok, State};
mainHandler(startInput, State) ->
    {ok, State#{t9window => newOptionWindow()}};
mainHandler(stopInput, #{t9window := T9Win} = State) ->
    deleteOptionWindow(T9Win),
    {ok, maps:without([t9window], State)};
mainHandler({wordsOptions, {[], Keys}}, State) ->
    drawWordOptions([Keys], State),
    {ok, State};
mainHandler({wordsOptions, {Options, _}}, State) ->
    drawWordOptions(Options, State),
    {ok, State};
mainHandler({insertCharacter, Str}, State) ->
    cecho:addstr(Str),
    cecho:refresh(),
    {ok, State};
mainHandler(deleteCharacter, State) ->
    %% todo
    {ok, State};
mainHandler({error, ErrorInfo}, State) ->
    showMessage(io_lib:format("error: ~w", [ErrorInfo])),
    {ok, State};
mainHandler(_, State) ->
    {ok, State}.

-spec mainLoop(#{}) -> no_return().
mainLoop(State) ->
    receive
        Anything ->
            case mainHandler(Anything, State) of
                {ok, NewState} ->
                    mainLoop(NewState);
                stopped ->
                    stopped
            end
    end.

-spec cleanup() -> ok.
cleanup() ->
    wordsvc:stop(),
    ok = application:stop(cecho),
    ok.

-spec fullScreen(char()) -> ok.
fullScreen(Char) ->
    {MRow, MCol} = cecho:getmaxyx(),
    Chars = lists:map(fun(_) -> Char end, lists:seq(1, MCol * MRow)),
    cecho:mvaddstr(0, 0, Chars),
    cecho:move(0, 0),
    cecho:refresh(),
    ok.

-spec start() -> no_return().
start() ->
    wordsvc:start_link(),
    ok = application:start(cecho),
    ok = cecho:noecho(),
%%    fullScreen($.),
    startKeyStrokeListener(),
    mainLoop(#{}).
