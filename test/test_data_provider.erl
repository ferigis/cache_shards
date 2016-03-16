%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of cs_data_provider behaviour for testing
%%% @end
%%%-------------------------------------------------------------------
-module(test_data_provider).

-behaviour(cs_data_provider).

%% API
-export([get/1]).

get(Key) ->
  Map = #{all => [{felipe, ripoll}, {chuck, norris}
                  , {bruce, wayne}, {peter, parker}]
        , felipe => {felipe, ripoll}
        , chuck => {chuck, norris}
        , batman => {bruce, wayne}
        , spiderman => {peter, parker}},
  case catch maps:get(Key, Map) of
    {'EXIT', _} -> [];
    Result -> Result
  end.
