-module(cfg).
-behaviour(gen_server).

-export([start_link/0]).

-export([push/1,
         pop/0,
         get/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

start_link() ->
    gen_server:start_link({local, cfg}, cfg, [], []).

pop() ->
    gen_server:call(cfg, pop).

push(X) ->
    gen_server:cast(cfg, {push, X}).

get() ->
    gen_server:call(cfg, get).

init(_Args) ->
    {ok, []}.

handle_call(pop, _From, []) ->
    undefined;
handle_call(pop, _From, [H | T]) ->
    {reply, H, T};
handle_call(get, _From, Stack) ->
    {reply, Stack, Stack}.

handle_cast({push, X}, Stack) ->
    {noreply, [X | Stack]}.
