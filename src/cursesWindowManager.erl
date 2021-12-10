-module(cursesWindowManager).
-export([init/1, handle_event/2, handle_call/2, terminate/2]).
-behaviour(gen_event).

-type windowHandle() :: any() | none.
-type cursesWindowManagerState() :: #{window => windowHandle()}.

-define(WINDOW_ROWS, 8).
-define(WINDOW_COLUMNS, 22).

%% when no words are found, show the keys directly
handle_event({wordsOptions, {[], Keys}}, #{window := WindowHandle} = State) ->
    drawWordOptions([Keys], WindowHandle),
    {ok, State};
handle_event({wordsOptions, {Options, _}}, #{window := WindowHandle} = State) ->
    drawWordOptions(Options, WindowHandle),
    {ok, State};
handle_event({insertString, String}, State) ->
    cecho:addstr(String),
    cecho:refresh(),
    {ok, State};
handle_event(startInput, State) ->
    {ok, State#{window => newOptionWindow()}};
handle_event(stopInput, #{window := WindowHandle} = State) when WindowHandle =/= none ->
    deleteOptionWindow(WindowHandle),
    {ok, State#{window := none}};
handle_event(stopInput, State) ->
    {ok, State};
handle_event(deleteCharacter, State) ->
    %% todo
    {ok, State};
handle_event({error, ErrorInfo}, State) ->
    showMessage(io_lib:format("error: ~w", [ErrorInfo])),
    {ok, State};
handle_event({command, Quit}, _) when Quit =:= quit; Quit =:= saveAndQuit ->
    remove_handler;
handle_event({command, Command}, State) ->
    showMessage(io_lib:format("command: ~w", [Command])),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

-spec init([]) -> {ok, cursesWindowManagerState()}.
init([]) ->
    ok = application:start(cecho),
    ok = cecho:noecho(),
    spawn_link(fun () -> keyListener() end),
    {ok, #{window => none}}.

terminate(_, _) ->
    ok = keyToWordService:stop(),
    ok = application:stop(cecho).

-spec keyListener() -> no_return().
keyListener() ->
    try t9InputUtil:preTranslate(cecho:getch()) of
        {ok, Character} ->
            inputStateMachine:newCharacter(Character);
        error ->
            ok
    catch
        exit:_ ->
            ok
    after
        keyListener()
    end.

-spec newOptionWindow() -> windowHandle().
newOptionWindow() ->
    {Row, Col} = cecho:getyx(),
    cecho:curs_set(0),
    W = cecho:newwin(?WINDOW_ROWS, ?WINDOW_COLUMNS, Row + 1, Col),
    cecho:wborder(W, $|, $|, $-, $-, $+, $+, $+, $+),
    W.

-spec deleteOptionWindow(windowHandle()) -> ok.
deleteOptionWindow(WindowHandle) ->
    cecho:werase(WindowHandle),
    cecho:wrefresh(WindowHandle),
    cecho:delwin(WindowHandle),
    cecho:refresh(),
    cecho:curs_set(1),
    ok.

-spec drawWordOptions([string()], windowHandle()) -> ok.
drawWordOptions(Options, WindowHandle) ->
    cecho:werase(WindowHandle),
    {Row, Col} = cecho:getyx(),
    drawWordOptionList(lists:sublist(Options, ?WINDOW_ROWS), WindowHandle),
    cecho:move(Row, Col),
    ok.

-spec drawWordOptionList([string()], windowHandle()) -> ok.
drawWordOptionList([Word | RestOptions], WindowHandle) ->
    {Row, _} = cecho:getyx(WindowHandle),
    cecho:waddstr(WindowHandle, Word),
    cecho:wmove(WindowHandle, Row + 1, 0),
    drawWordOptionList(RestOptions, WindowHandle);
drawWordOptionList([], WindowHandle) ->
    cecho:wmove(WindowHandle, 0, 0),
    cecho:wrefresh(WindowHandle),
    ok.

-spec showMessage(string()) -> ok.
showMessage(MessageString) ->
    {MaxRowNumber, _} = cecho:getmaxyx(),
    {Row, Column} = cecho:getyx(),
    cecho:mvaddstr(MaxRowNumber - 1, 0, MessageString),
    cecho:move(Row, Column),
    cecho:refresh(),
    ok.
