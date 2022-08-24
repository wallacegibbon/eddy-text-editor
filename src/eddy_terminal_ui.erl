-module(eddy_terminal_ui).
-export([init/1, handle_event/2, handle_call/2, terminate/2]).
-behaviour(gen_event).

-type window_handle() :: any() | none.

-type state() :: #{window => window_handle()}.

-define(WINDOW_ROWS, 8).
-define(WINDOW_COLUMNS, 22).

%% when no words are found, show the keys directly
handle_event(
	{word_options, {[], Keys}},
	#{window := WindowHandle} = State
) ->
	draw_word_options([Keys], WindowHandle),
	{ok, State};
handle_event(
	{word_options, {Options, _}},
	#{window := WindowHandle} = State
) ->
	draw_word_options(Options, WindowHandle),
	{ok, State};
handle_event({insert_str, String}, State) ->
	cecho:addstr(String),
	cecho:refresh(),
	{ok, State};
handle_event(start_input, State) ->
	{ok, State#{window => new_option_window()}};
handle_event(stop_input, #{window := WindowHandle} = State)
		when WindowHandle =/= none ->
	del_option_window(WindowHandle),
	{ok, State#{window := none}};
handle_event(stop_input, State) ->
	{ok, State};
handle_event(del_char, State) ->
	%% todo
	{ok, State};
handle_event({error, ErrorInfo}, State) ->
	show_message(io_lib:format("error: ~w", [ErrorInfo])),
	{ok, State};
handle_event({command, save_and_quit}, _) ->
	remove_handler;
handle_event({command, quit}, _) ->
	remove_handler;
handle_event({command, Command}, State) ->
	show_message(io_lib:format("command: ~w", [Command])),
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

-spec init([]) -> {ok, state()}.
init([]) ->
	ok = application:start(cecho),
	ok = cecho:noecho(),
	spawn_link(fun () -> key_listener_loop() end),
	{ok, #{window => none}}.

terminate(_, _) ->
	ok = eddy_t9_translator:stop(),
	ok = application:stop(cecho).

-spec key_listener_loop() -> no_return().
key_listener_loop() ->
	try eddy_keystroke:pre_translate(cecho:getch()) of
		{ok, Character} ->
			eddy_state_machine:feed_char(Character);
		error ->
			ok
	catch
		exit:_ ->
			ok
	after
		key_listener_loop()
	end.

-spec new_option_window() -> window_handle().
new_option_window() ->
	{Row, Col} = cecho:getyx(),
	cecho:curs_set(0),
	W = cecho:newwin(?WINDOW_ROWS, ?WINDOW_COLUMNS, Row + 1, Col),
	cecho:wborder(W, $|, $|, $-, $-, $+, $+, $+, $+),
	W.

-spec del_option_window(window_handle()) -> ok.
del_option_window(WindowHandle) ->
	cecho:werase(WindowHandle),
	cecho:wrefresh(WindowHandle),
	cecho:delwin(WindowHandle),
	cecho:refresh(),
	cecho:curs_set(1),
	ok.

-spec draw_word_options([string()], window_handle()) -> ok.
draw_word_options(Options, WindowHandle) ->
	cecho:werase(WindowHandle),
	{Row, Col} = cecho:getyx(),
	draw_option_list(lists:sublist(Options, ?WINDOW_ROWS), WindowHandle),
	cecho:move(Row, Col),
	ok.

-spec draw_option_list([string()], window_handle()) -> ok.
draw_option_list([Word | RestOptions], WindowHandle) ->
	{Row, _} = cecho:getyx(WindowHandle),
	cecho:waddstr(WindowHandle, Word),
	cecho:wmove(WindowHandle, Row + 1, 0),
	draw_option_list(RestOptions, WindowHandle);
draw_option_list([], WindowHandle) ->
	cecho:wmove(WindowHandle, 0, 0),
	cecho:wrefresh(WindowHandle),
	ok.

-spec show_message(string()) -> ok.
show_message(MessageString) ->
	{MaxRowNumber, _} = cecho:getmaxyx(),
	{Row, Column} = cecho:getyx(),
	cecho:mvaddstr(MaxRowNumber - 1, 0, MessageString),
	cecho:move(Row, Column),
	cecho:refresh(),
	ok.

