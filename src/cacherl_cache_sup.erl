%%%-------------------------------------------------------------------
%%% @doc
%%% Cache supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(cacherl_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([supervisor_name/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link(atom(), atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CacheName, Module) ->
  supervisor:start_link({local, supervisor_name(CacheName)}, ?MODULE, [CacheName, Module]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([CacheName, Module]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {cacherl_cache_owner, {cacherl_cache_owner, start_link, [CacheName, Module]},
    Restart, Shutdown, Type, [cacherl_cache_owner]},

  {ok, {SupFlags, [AChild]}}.

%% @doc
%% Provides the supervisor name regarding the cache name.
%% @end
-spec(supervisor_name(atom()) -> atom()).
supervisor_name(CacheName) ->
  Bin = <<(atom_to_binary(CacheName, utf8))/binary, "_",
    (atom_to_binary(sup, utf8))/binary>>,
  binary_to_atom(Bin, utf8).
