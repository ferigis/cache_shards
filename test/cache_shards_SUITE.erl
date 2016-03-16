-module(cache_shards_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([new_cache/1]).
-export([remove_cache/1]).
-export([common_case/1]).
-export([change_generation/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [new_cache, remove_cache,
    change_generation, common_case].

init_per_suite(Config) ->
  cache_shards:start(),
  Config.

end_per_suite(Config) ->
  ok = cache_shards:stop(),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

new_cache(_Config) ->
  {ok, Pid} = cache_shards:new(new_cache, module, []),
  {ok, _Pid2} = cache_shards:new(new_cache2, module2, []),
  {already_exists, Pid} = cache_shards:new(new_cache, module, []),
  true = is_process_alive(Pid).

remove_cache(_Config) ->
  {already_exists, _} = cache_shards:new(new_cache, module, []),
  {already_exists, _} = cache_shards:new(new_cache2, module2, []),
  true = cache_shards:remove(new_cache),
  true = cache_shards:remove(new_cache2),
  {ok, _} = cache_shards:new(new_cache, module2, []),
  true = cache_shards:remove(new_cache).

change_generation(_Config) ->
  CacheName = test_cache,
  {ok, _Pid} = cache_shards:new(CacheName, test_data_provider, []),
  [{generation, Generation}] = ets:lookup(CacheName, generation),
  ShardsName = cs_utils:shards_name(CacheName, Generation),
  true = is_process_alive(whereis(ShardsName)),
  Generation2 = cache_shards:increment_generation(CacheName),
  undefined = whereis(ShardsName),
  [{generation, Generation2}] = ets:lookup(CacheName, generation),
  ShardsName2 = cs_utils:shards_name(CacheName, Generation2),
  true = is_process_alive(whereis(ShardsName2)),
  true = cache_shards:remove(CacheName).

common_case(_Config) ->
  CacheName = test_cache,
  {ok, _Pid} = cache_shards:new(CacheName, test_data_provider, []),
  [{generation, Generation}] = ets:lookup(CacheName, generation),
  ShardsName = cs_utils:shards_name(CacheName, Generation),
  Key = felipe,
  ExpectedValue = {felipe, ripoll},
  [] = shards:lookup(ShardsName, Key),
  ExpectedValue = cache_shards:get(CacheName, Key),
  [{generation, Generation}] = ets:lookup(CacheName, generation),
  [{Key, ExpectedValue}] = shards:lookup(ShardsName, Key),
  Key2 = this_key_doesnt_exists,
  ExpectedValue2 = [],
  [] = shards:lookup(ShardsName, Key2),
  ExpectedValue2 = cache_shards:get(CacheName, Key2),
  [{generation, Generation}] = ets:lookup(CacheName, generation),
  [{Key2, ExpectedValue2}] = shards:lookup(ShardsName, Key2),
  true = cache_shards:remove(CacheName).