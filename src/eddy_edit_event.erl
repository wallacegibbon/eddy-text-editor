-module(eddy_edit_event).
-export([start/0, add_handler/1, delete_handler/1, publish/1]).

-define(SERVER, ?MODULE).

start() ->
    gen_event:start({local, ?SERVER}).

add_handler(Handler) ->
    gen_event:add_handler(?SERVER, Handler, []).

delete_handler(Handler) ->
    gen_event:delete_handler(?SERVER, Handler, []).

publish(Message) ->
    gen_event:notify(?SERVER, Message).
