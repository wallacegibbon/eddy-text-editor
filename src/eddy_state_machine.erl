-module(eddy_state_machine).
-export([init/1, callback_mode/0, handle_event/4, start_link/0, feed_char/1]).
-behaviour(gen_statem).
-define(SERVER, ?MODULE).

-type eddy_mode() ::
	t9_start | t9_insert | change_map | wait_command_1 | wait_command_2.

-type keys_and_word_options() :: {
	Keys :: [eddy_keystroke:key()],
	Options :: [string()]
}.

-type command_mode_data() :: {
	Keys :: [eddy_keystroke:key()],
	PrevState :: eddy_mode(),
	PrevData :: state_data()
}.

-type map_index() :: integer().

-type state_data() ::
	keys_and_word_options() | map_index() | command_mode_data().


-spec handle_event(cast, eddy_keystroke:key(), eddy_mode(), state_data())
	->
		{next_state, eddy_mode(), state_data()} |
		{keep_state, state_data()} |
		keep_state_and_data.

handle_event(cast, command_key, State, Data)
	when State =/= wait_command_1, State =/= wait_command_2 ->
	{next_state, wait_command_1, {[command], State, Data}};
%% In command state (wait_command_1 or wait_command_2),
%% clicking on the command key again will reset the state to wait_command_1
handle_event(cast, command_key, _, {_, PrevState, PrevData}) ->
	{next_state, wait_command_1, {[], PrevState, PrevData}};
handle_event(cast, Key, wait_command_1, {_, PrevState, PrevData}) ->
	{next_state, wait_command_2, {[Key], PrevState, PrevData}};
handle_event(cast, Key2, wait_command_2, {[Key1], PrevState, PrevData}) ->
	eddy_edit_event:publish(
		{command, eddy_keystroke:translate_command(Key1, Key2)}
	),
	{next_state, PrevState, PrevData};
%% Go back to t9 insert mode by double click on <MAP CHANGE> key.
handle_event(cast, change_map_key, change_map, _) ->
	eddy_edit_event:publish(stop_input),
	{next_state, t9_start, {[], []}};
handle_event(cast, change_map_key, _, Data) ->
	{next_state, change_map, Data};
handle_event(cast, Key, change_map, _) when Key >= $1, Key =< $9 ->
	eddy_edit_event:publish(stop_input),
	{next_state, direct_insert, Key - $0};
handle_event(cast, _, change_map, Data) ->
	keep_state_and_data;
%% In direct-insert mode, the data is just an integer (key map index)
handle_event(cast, Key, direct_insert, MapIndex) ->
	eddy_edit_event:publish(
		{insert_str, [eddy_keystroke:translate_key(Key, MapIndex)]}
	),
	keep_state_and_data;
%% The t9 input method logic
handle_event(cast, Key, t9_start, _) when Key >= $2, Key =< $9 ->
	eddy_edit_event:publish(start_input),
	Options = eddy_t9_translator:query([Key]),
	sync_word_options(Options, [Key]),
	{next_state, t9_insert, {[Key], Options}};
handle_event(cast, $\b, t9_start, Data) ->
	keep_state_and_data;
handle_event(cast, Key, t9_insert, {CollectedKeys, _})
	when Key >= $2, Key =< $9 ->
	Keys = [Key | CollectedKeys],
	Options = eddy_t9_translator:query(lists:reverse(Keys)),
	sync_word_options(Options, Keys),
	{keep_state, {Keys, Options}};
handle_event(cast, $1, t9_insert, {Keys, [W | RestWords]}) ->
	Options = RestWords ++ [W],
	sync_word_options(Options, Keys),
	{keep_state, {Keys, Options}};
handle_event(cast, $1, t9_insert, {[], []} = Data) ->
	keep_state_and_data;
%% when the word is selected, empty the word list and options
handle_event(cast, Key, t9_insert, {_, [Word | _]})
	when Key =:= $\s; Key =:= $\n ->
	eddy_edit_event:publish({insert_str, Word}),
	eddy_edit_event:publish(stop_input),
	eddy_t9_translator:freq_count(list_to_binary(Word)),
	{next_state, t9_start, {[], []}};
handle_event(cast, $\b, t9_insert, {[], _}) ->
	{next_state, t9_start, {[], []}};
handle_event(cast, $\b, State, Data)
	when State =/= t9_insert, State =/= t9_start ->
	eddy_edit_event:publish(del_char),
	{next_state, State, Data};
handle_event(cast, $\b, t9_insert, {[_], _}) ->
	eddy_edit_event:publish(stop_input),
	{next_state, t9_start, {[], []}};
handle_event(cast, $\b, t9_insert, {[_ | Keys], _}) ->
	Options = eddy_t9_translator:query(lists:reverse(Keys)),
	sync_word_options(Options, Keys),
	{keep_state, {Keys, Options}};
%% In other cases, space and newline key stands for themselves
handle_event(cast, Key, State, Data) when Key =:= $\s; Key =:= $\n ->
	eddy_edit_event:publish({insert_str, [Key]}),
	{next_state, State, Data};
%% There should not be other situations.
handle_event(cast, Key, State, Data) ->
	eddy_edit_event:publish({error, {Key, State, Data}}),
	{next_state, t9_start, {[], []}}.

-spec sync_word_options([string()], [eddy_keystroke:key()]) -> ok.
sync_word_options(WordOptions, Keys) ->
	eddy_edit_event:publish(
		{word_options, {WordOptions, lists:reverse(Keys)}}
	).

-spec init([]) -> {ok, eddy_mode(), state_data()}.
init([]) ->
	{ok, t9_start, {[], []}}.

callback_mode() ->
	handle_event_function.

start_link() ->
	gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

feed_char(Char) ->
	gen_statem:cast(?SERVER, Char).

