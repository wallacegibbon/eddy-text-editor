-module(editEventManager).

-export([start/0, add_handler/1, delete_handler/1, publish/1]).

start() ->
    gen_event:start({local, ?MODULE}).

add_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

delete_handler(Handler) ->
    gen_event:delete_handler(?MODULE, Handler, []).

publish(Message) ->
    gen_event:notify(?MODULE, Message).
