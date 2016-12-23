%%%-------------------------------------------------------------------
%%% @doc
%%% This is the main module, which contains all API functions.
%%% It also implements the application behaviour
%%% @end
%%%-------------------------------------------------------------------
-module(cacherl).
-behaviour(application).

%% API
-export([new/2]).
-export([remove/1]).

%% Application callbacks
-export([start/0, start/2]).
-export([stop/0, stop/1]).
-export([get/2]).
-export([increment_generation/1]).

-type generation()  ::  integer().

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  cacherl_sup:start_link().

%% @hidden
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%% @doc Starts `cacherl' application.
-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(cacherl).

%% @doc Stops `cacherl' application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(cacherl).

%%%===================================================================
%%% cacherl API
%%%===================================================================

-spec(new(atom(), atom()) ->
  {ok, pid()} | {already_exists, pid()}).
new(CacheName, Module) ->
  case cacherl_sup:start_child([CacheName, Module]) of
    {error,{already_started, Pid}} -> {already_exists, Pid};
    R -> R
  end.

-spec(remove(atom()) -> true).
remove(CacheName) ->
  ok = cacherl_sup:terminate_child(cacherl_sup,
            whereis(cacherl_cache_sup:supervisor_name(CacheName))),
  true.

-spec(get(atom(), atom()) -> [] | [term()]).
get(CacheName, Key) ->
  [{state, #{generation := Generation, module := Module}}] = ets:lookup(CacheName, state),
  ShardsName = cacherl_utils:shards_name(CacheName, Generation),
  case shards:lookup(ShardsName, Key) of
    [{Key, Result}] ->
      Result;
    [] ->
      Result = apply(Module, get, [Key]),
      catch shards:insert(ShardsName, {Key, Result}),
      Result
  end.

-spec(increment_generation(atom()) -> generation()).
increment_generation(CacheName) ->
  cacherl_cache_owner:increment_generation(CacheName).
