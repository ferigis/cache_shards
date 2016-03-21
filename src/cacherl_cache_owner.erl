%%%-------------------------------------------------------------------
%%% @doc
%%% Cache owner.
%%% @end
%%%-------------------------------------------------------------------
-module(cacherl_cache_owner).

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([increment_generation/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

% Macros
-define(SHARDS_POOL_SIZE, 4).

% State
-record(state, {generation  :: cacherl:generation()
              , cache_name  :: atom()
              , options     :: list()}).


%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(atom(), atom(), list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CacheName, Module, Options) ->
  gen_server:start_link({local, CacheName}
                        , ?MODULE
                        , [CacheName, Module, Options]
                        , []).

-spec(increment_generation(atom()) -> cacherl:generation()).
increment_generation(CacheName) ->
  gen_server:call(CacheName, increment_generation).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([CacheName, Module, Options]) ->
  process_flag(trap_exit, true),
  Generation = 1,
  CacheName = ets:new(CacheName, [set, named_table, {read_concurrency, true}]),
  true = ets:insert(CacheName, {cache_name, CacheName}),
  true = ets:insert(CacheName, {generation, Generation}),
  true = ets:insert(CacheName, {module, Module}),
  true = ets:insert(CacheName, {options, Options}),
  ShardsName = cacherl_utils:shards_name(CacheName, Generation),
  shards:new(ShardsName, [], shards_pool_size(Options)),
  {ok, #state{generation = 1, cache_name = CacheName, options = Options}}.

%% @hidden
handle_call(increment_generation, _From, #state{cache_name = CacheName
                                      , options = Options
                                      , generation = OldGeneration} = State) ->
  OldShardsName = cacherl_utils:shards_name(CacheName, OldGeneration),
  NewShardsName = cacherl_utils:shards_name(CacheName, OldGeneration + 1),
  shards:new(NewShardsName, [], shards_pool_size(Options)),
  NewGeneration = ets:update_counter(CacheName, generation, 1),
  shards:delete(OldShardsName),
  {reply, NewGeneration, State#state{generation = NewGeneration}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, #state{generation = Generation
                        , cache_name = CacheName} = _State) ->
  ShardsName = cacherl_utils:shards_name(CacheName, Generation),
  shards:delete(ShardsName),
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(shards_pool_size(list()) -> integer()).
shards_pool_size(Opts) ->
  case proplists:get_value(shards_pool_size, Opts) of
    undefined -> ?SHARDS_POOL_SIZE;
    PoolSize -> PoolSize
  end.