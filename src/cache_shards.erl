%%%-------------------------------------------------------------------
%%% @doc
%%% This is the main module, which contains all API functions.
%%% It also implements te
%%% @end
%%%-------------------------------------------------------------------
-module(cache_shards).
-behaviour(application).

%% API
-export([new/3]).
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
  case cs_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

%% @hidden
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%% @doc Starts `cache_shards' application.
-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(cache_shards).

%% @doc Stops `cache_shards' application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(cache_shards).

%%%===================================================================
%%% cache_shards API
%%%===================================================================

-spec(new(atom(), atom(), list()) ->
  {ok, pid()} | {already_exists, pid()}).
new(CacheName, Module, Options) ->
  case cs_sup:start_child([CacheName, Module, Options]) of
    {error,{already_started, Pid}} -> {already_exists, Pid};
    R -> R
  end.

-spec(remove(atom()) -> true).
remove(CacheName) ->
  ok = cs_sup:terminate_child(cs_sup,
            whereis(cs_cache_sup:supervisor_name(CacheName))),
  true.

-spec(get(atom(), atom()) -> [] | [term()]).
get(CacheName, Key) ->
  [{generation, Generation}] = ets:lookup(CacheName, generation),
  [{module, Module}] = ets:lookup(CacheName, module),
  ShardsName = cs_utils:shards_name(CacheName, Generation),
  case catch shards:lookup(ShardsName, Key) of
    [{Key, Result}] -> Result;
    []          ->
      Result = apply(Module, get, [Key]),
      catch shards:insert(ShardsName, {Key, Result}),
      Result;
    {'EXIT', _} ->
      Result = apply(Module, get, [Key]),
      catch shards:insert(ShardsName, {Key, Result}),
      Result
  end.

-spec(increment_generation(atom()) -> generation()).
increment_generation(CacheName) ->
  cs_cache_server:increment_generation(CacheName).


