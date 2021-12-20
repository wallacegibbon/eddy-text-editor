-module(eddy_state_machine).
-export([init/1, callback_mode/0, handle_event/4, start_link/0, feed_character/1]).
-behaviour(gen_statem).
-include("./eddy_keystroke.hrl").

-define(SERVER, ?MODULE).

-type keys_and_word_options() :: {Keys :: [eddy_key()], Options :: [string()]}.
-type command_mode_data() :: {Keys :: [eddy_key()], PreviousState :: eddy_mode(), PreviousData :: state_data()}.
-type map_index() :: integer().

-type state_data() :: keys_and_word_options() | map_index() | command_mode_data().

-spec handle_event(cast, eddy_key(), eddy_mode(), state_data()) -> {next_state, eddy_mode(), state_data()}.
handle_event(cast, command_key, State, Data) when State =/= wait_command_1, State =/= wait_command_2 ->
    {next_state, wait_command_1, {[command], State, Data}};
%% When in command state (wait_command_1 or wait_command_2), clicking on the command key again will reset the state to wait_command_1
handle_event(cast, command_key, _, {_, PreviousState, PreviousData}) ->
    {next_state, wait_command_1, {[], PreviousState, PreviousData}};
handle_event(cast, Key, wait_command_1, {_, PreviousState, PreviousData}) ->
    {next_state, wait_command_2, {[Key], PreviousState, PreviousData}};
handle_event(cast, Key2, wait_command_2, {[Key1], PreviousState, PreviousData}) ->
    eddy_edit_event:publish({command, eddy_keystroke:translate_command(Key1, Key2)}),
    {next_state, PreviousState, PreviousData};

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
    {next_state, change_map, Data};

%% In direct-insert mode, the data is just an integer (key map index)
handle_event(cast, Key, direct_insert, MapIndex) ->
    eddy_edit_event:publish({insert_string, [eddy_keystroke:translate_key(Key, MapIndex)]}),
    {next_state, direct_insert, MapIndex};

%% The t9 input method logic
handle_event(cast, Key, t9_start, _) when Key >= $2, Key =< $9 ->
    eddy_edit_event:publish(start_input),
    Options = eddy_t9_translator:query([Key]),
    sync_word_options(Options, [Key]),
    {next_state, t9_insert, {[Key], Options}};
handle_event(cast, $\b, t9_start, Data) ->
    {next_state, t9_start, Data};
handle_event(cast, Key, t9_insert, {CollectedKeys, _}) when Key >= $2, Key =< $9 ->
    Keys = [Key | CollectedKeys],
    Options = eddy_t9_translator:query(lists:reverse(Keys)),
    sync_word_options(Options, Keys),
    {next_state, t9_insert, {Keys, Options}};
handle_event(cast, $1, t9_insert, {Keys, [W | RestWords]}) ->
    Options = RestWords ++ [W],
    sync_word_options(Options, Keys),
    {next_state, t9_insert, {Keys, Options}};
handle_event(cast, $1, t9_insert, {[], []} = Data) ->
    {next_state, t9_insert, Data};
%% when the word is selected, empty the word list and options
handle_event(cast, Key, t9_insert, {_, [Word | _]}) when Key =:= $\s; Key =:= $\n ->
    eddy_edit_event:publish({insert_string, Word}),
    eddy_edit_event:publish(stop_input),
    eddy_t9_translator:frequency_count(list_to_binary(Word)),
    {next_state, t9_start, {[], []}};
handle_event(cast, $\b, t9_insert, {[], _}) ->
    {next_state, t9_start, {[], []}};
handle_event(cast, $\b, State, Data) when State =/= t9_insert, State =/= t9_start ->
    eddy_edit_event:publish(delete_character),
    {next_state, State, Data};
handle_event(cast, $\b, t9_insert, {[_], _}) ->
    eddy_edit_event:publish(stop_input),
    {next_state, t9_start, {[], []}};
handle_event(cast, $\b, t9_insert, {[_ | Keys], _}) ->
    Options = eddy_t9_translator:query(lists:reverse(Keys)),
    sync_word_options(Options, Keys),
    {next_state, t9_insert, {Keys, Options}};

%% In other cases, space and newline key stands for themselves
handle_event(cast, Key, State, Data) when Key =:= $\s; Key =:= $\n ->
    eddy_edit_event:publish({insert_string, [Key]}),
    {next_state, State, Data};

%% There should not be other situations.
handle_event(cast, Key, State, Data) ->
    eddy_edit_event:publish({error, {Key, State, Data}}),
    {next_state, t9_start, {[], []}}.

-spec sync_word_options([string()], [eddy_key()]) -> ok.
sync_word_options(WordOptionList, Keys) ->
    eddy_edit_event:publish({word_options, {WordOptionList, lists:reverse(Keys)}}).

-spec init([]) -> {ok, eddy_mode(), state_data()}.
init([]) ->
    {ok, t9_start, {[], []}}.

callback_mode() ->
    handle_event_function.

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

feed_character(Character) ->
    gen_statem:cast(?SERVER, Character).
