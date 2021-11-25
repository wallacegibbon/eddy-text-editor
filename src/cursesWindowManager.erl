-module(cursesWindowManager).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/0]).

-define(SERVER, ?MODULE).

-type windowHandle() :: any() | none.
-record(cursesWindowManagerState, {window = none :: windowHandle()}).

-define(WINDOW_ROWS, 5).
-define(WINDOW_COLUMNS, 22).

handle_cast(startInput, #cursesWindowManagerState{} = State) ->
    {noreply, State#cursesWindowManagerState{window = newOptionWindow()}};
handle_cast(stopInput, #cursesWindowManagerState{window = WindowHandle} = State) ->
    deleteOptionWindow(WindowHandle),
    {noreply, State#cursesWindowManagerState{window = none}};
handle_cast({wordsOptions, {Options, _}}, #cursesWindowManagerState{window = WindowHandle} = State) ->
    drawWordOptions(Options, WindowHandle),
    {noreply, State};
%% when no words are found, show the keys directly
handle_cast({wordsOptions, {[], Keys}}, #cursesWindowManagerState{window = WindowHandle} = State) ->
    drawWordOptions([Keys], WindowHandle),
    {noreply, State};
handle_cast({insertString, String}, #cursesWindowManagerState{} = State) ->
    cecho:addstr(String),
    cecho:refresh(),
    {noreply, State};
handle_cast(deleteCharacter, State) ->
    %% todo
    {noreply, State};
handle_cast({error, ErrorInfo}, State) ->
    showMessage(io_lib:format("error: ~w", [ErrorInfo])),
    {noreply, State};
handle_cast({command, Quit}, #cursesWindowManagerState{} = State) when Quit =:= quit; Quit =:= saveAndQuit ->
    ok = application:stop(cecho),
    {stop, normal, State};
handle_cast({command, Command}, #cursesWindowManagerState{} = State) ->
    showMessage(io_lib:format("command: ~w", [Command])),
    {noreply, State}.

handle_call(_Request, _From, #cursesWindowManagerState{} = State) ->
    {reply, ok, State}.

init([]) ->
    ok = application:start(cecho),
    ok = cecho:noecho(),
    spawn_link(fun () -> keyListener() end),
    {ok, #cursesWindowManagerState{}}.

-spec keyListener() -> ok.
keyListener() ->
    try t9InputUtil:preTranslate(cecho:getch()) of
        {ok, Character} ->
            gen_statem:cast(inputStateMachine, Character);
        error ->
            ok
    catch
        exit:_ ->
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec newOptionWindow() -> windowHandle().
newOptionWindow() ->
    {Row, Col} = cecho:getyx(),
    cecho:curs_set(0),
    W = cecho:newwin(?WINDOW_ROWS, ?WINDOW_COLUMNS, Row+1, Col),
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
    cecho:wmove(WindowHandle, Row+1, 0),
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
