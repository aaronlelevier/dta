%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc raaw impl of bike_mod - only contains this interface
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, product_map_target/0, variant_inventory_diffs/1]).
-include_lib("dta/include/records.hrl").


-spec urls() -> [dta_types:url()].
urls() ->
  raaw_urls:urls().

-spec product_map_target() -> {binary(), binary()}.
product_map_target() ->
  {<<"id">>, <<"ProductJson-1">>}.

-spec variant_inventory_diffs(#request{}) -> [#variant_inventory_diff{}].
variant_inventory_diffs(Req) ->
  raaw_variant_inventory:diffs(Req).
