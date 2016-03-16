%%%-------------------------------------------------------------------
%%% @doc
%%% Cache Utils
%%% @end
%%%-------------------------------------------------------------------
-module(cs_utils).

%% API
-export([shards_name/2]).

%% @doc
%% Build the name of the Shard regarding the Cache name and the
%% generation.
%% @end
-spec(shards_name(atom(), cache_shards:generation()) -> atom()).
shards_name(CacheName, Generation) ->
  Bin = <<(atom_to_binary(cs_shards, utf8))/binary, "_",
    (atom_to_binary(CacheName, utf8))/binary, "_",
    (integer_to_binary(Generation))/binary>>,
  binary_to_atom(Bin, utf8).