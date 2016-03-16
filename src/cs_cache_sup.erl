%%%-------------------------------------------------------------------
%%% @doc
%%% Cache supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(cs_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).
-export([supervisor_name/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link(atom(), atom(), list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CacheName, Module, Options) ->
  supervisor:start_link({local, supervisor_name(CacheName)}, ?MODULE, [CacheName, Module, Options]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([CacheName, Module, Options]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {cs_cache_server, {cs_cache_server, start_link, [CacheName, Module, Options]},
    Restart, Shutdown, Type, [cs_cache_server]},

  {ok, {SupFlags, [AChild]}}.

%% @doc
%% Provides the supervisor name regarding the cache name.
%% @end
-spec(supervisor_name(atom()) -> atom()).
supervisor_name(CacheName) ->
  Bin = <<(atom_to_binary(CacheName, utf8))/binary, "_",
    (atom_to_binary(sup, utf8))/binary>>,
  binary_to_atom(Bin, utf8).
