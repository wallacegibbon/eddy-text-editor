-module(editEventManager).

-export([start_link/0, add_handler/1, delete_handler/1, publish/1]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler) ->
    gen_event:add_handler(?SERVER, Handler, []).

delete_handler(Handler) ->
    gen_event:delete_handler(?SERVER, Handler, []).

publish(Message) ->
    gen_event:notify(?SERVER, Message).
