-module(cursesWindowManager).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, start_link/0]).

-define(SERVER, ?MODULE).

-type windowHandle() :: any() | none.
-record(cursesWindowManagerState, {window = none :: windowHandle()}).

-define(WINDOW_ROWS, 8).
-define(WINDOW_COLUMNS, 22).

handle_event(startInput, State) ->
    {ok, State#cursesWindowManagerState{window = newOptionWindow()}};
handle_event(stopInput, #cursesWindowManagerState{window = WindowHandle} = State) when WindowHandle =/= none ->
    deleteOptionWindow(WindowHandle),
    {ok, State#cursesWindowManagerState{window = none}};
handle_event(stopInput, State) ->
    {ok, State};
%% when no words are found, show the keys directly
handle_event({wordsOptions, {[], Keys}}, #cursesWindowManagerState{window = WindowHandle} = State) ->
    drawWordOptions([Keys], WindowHandle),
    {ok, State};
handle_event({wordsOptions, {Options, _}}, #cursesWindowManagerState{window = WindowHandle} = State) ->
    drawWordOptions(Options, WindowHandle),
    {ok, State};
handle_event({insertString, String}, State) ->
    cecho:addstr(String),
    cecho:refresh(),
    {ok, State};
handle_event(deleteCharacter, State) ->
    %% todo
    {ok, State};
handle_event({error, ErrorInfo}, State) ->
    showMessage(io_lib:format("error: ~w", [ErrorInfo])),
    {ok, State};
handle_event({command, Quit}, State) when Quit =:= quit; Quit =:= saveAndQuit ->
    ok = application:stop(cecho),
    {stop, normal, State};
handle_event({command, Command}, State) ->
    showMessage(io_lib:format("command: ~w", [Command])),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

init([]) ->
    ok = application:start(cecho),
    ok = cecho:noecho(),
    spawn_link(fun () -> keyListener() end),
    {ok, #cursesWindowManagerState{}}.

-spec keyListener() -> no_return().
keyListener() ->
    try t9InputUtil:preTranslate(cecho:getch()) of
        {ok, Character} ->
            gen_statem:cast(inputStateMachine, Character);
        error ->
            ok
    catch
        exit:_ ->
            ok
    after
        keyListener()
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec newOptionWindow() -> windowHandle().
newOptionWindow() ->
    {Row, Col} = cecho:getyx(),
    cecho:curs_set(0),
    W = cecho:newwin(?WINDOW_ROWS, ?WINDOW_COLUMNS, Row + 1, Col),
    cecho:wborder(W, $|, $|, $-, $-, $+, $+, $+, $+),
    W.

-spec deleteOptionWindow(windowHandle()) -> ok.
deleteOptionWindow(T9Win) ->
    cecho:delwin(T9Win),
    cecho:curs_set(1),
    ok.
-spec drawWordOptions([string()], windowHandle()) -> ok.
drawWordOptions(Options, WindowHandle) ->
    {Row, Col} = cecho:getyx(),
    cecho:werase(WindowHandle),
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
