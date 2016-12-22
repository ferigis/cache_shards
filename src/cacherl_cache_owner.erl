%%%-------------------------------------------------------------------
%%% @doc
%%% Cache owner.
%%% @end
%%%-------------------------------------------------------------------
-module(cacherl_cache_owner).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([increment_generation/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

% State
-record(state, { generation  :: cacherl:generation()
               , cache_name  :: atom()}).


%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(atom(), atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CacheName, Module) ->
  gen_server:start_link( {local, CacheName}
                       , ?MODULE
                       , [CacheName, Module]
                       , []).

-spec(increment_generation(atom()) -> cacherl:generation()).
increment_generation(CacheName) ->
  gen_server:call(CacheName, increment_generation).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([CacheName, Module]) ->
  process_flag(trap_exit, true),
  Generation = 1,
  CacheName = ets:new(CacheName, [set, named_table, {read_concurrency, true}]),
  State = #{cache_name => CacheName
            , generation => Generation
            , module => Module},
  true = ets:insert(CacheName, {state, State}),
  ShardsName = cacherl_utils:shards_name(CacheName, Generation),
  shards:new(ShardsName, [{read_concurrency, true}]),
  {ok, #state{generation = 1, cache_name = CacheName}}.

%% @hidden
handle_call(increment_generation, _From, #state{cache_name = CacheName
                                      , generation = OldGeneration} = State) ->
  NewGeneration = OldGeneration + 1,
  OldShardsName = cacherl_utils:shards_name(CacheName, OldGeneration),
  NewShardsName = cacherl_utils:shards_name(CacheName, NewGeneration),
  shards:new(NewShardsName, [{read_concurrency, true}]),
  [{state, Metadata}] = ets:lookup(CacheName, state),
  Metadata2 = Metadata#{generation := NewGeneration},
  true = ets:insert(CacheName, {state, Metadata2}),
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
