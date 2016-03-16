%%%-------------------------------------------------------------------
%%% @doc
%%% Main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(cs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).
-export([terminate_child/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child([term()]) -> supervisor:startchild_ret().
start_child(Args) ->
  supervisor:start_child(?MODULE, Args).

-spec terminate_child(SupRef, Id) -> Response when
  SupRef   :: supervisor:sup_ref(),
  Id       :: pid() | supervisor:child_id(),
  Error    :: not_found | simple_one_for_one,
  Response :: ok | {error, Error}.
terminate_child(SupRef, Id) ->
  supervisor:terminate_child(SupRef, Id).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  ChildSpec = {
    ?MODULE,
    {cs_cache_sup, start_link, []},
    permanent,
    infinity,
    supervisor,
    [cs_cache_sup]
  },
  {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.
