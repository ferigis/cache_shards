%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour must be implemented by the modules which provide
%%% access to the data. Basically, where the app will lookup if some
%%% key is not in the cache.
%%% @end
%%%-------------------------------------------------------------------
-module(cs_data_provider).

-callback get(Key :: term()) -> []|list(term()).
