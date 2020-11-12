-module(sup).

%% This shows that you can add children dynamically
%% to a one-for-one supervisor.

-behaviour(supervisor).

%% API functions
-export([start_child/1]).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Args, Type), {Id, {Mod, start_link, Args},
                                     permanent, 3000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_child(Id) ->
    supervisor:start_child(?MODULE, ?CHILD(Id, gs, [Id], worker)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %{ok, {{one_for_one, 5, 10}, [?CHILD('SomeChild', 'gs', worker, [])]}}.
    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
