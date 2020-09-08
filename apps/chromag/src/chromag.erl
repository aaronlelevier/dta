%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc chromag impl of bike_mod - only contains this interface
%%%
%%% @end
%%% Created : 02. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, product_map_target/0, variant_inventory_diffs/1]).
-include_lib("dta/include/records.hrl").


-spec urls() -> [dta_types:url()].
urls() ->
  chromag_urls:urls().

-spec product_map_target() -> {binary(), binary()}.
product_map_target() ->
  {<<"data-product-json">>, <<"data-product-json">>}.

-spec variant_inventory_diffs(#request{}) -> [#variant_inventory_diff{}].
variant_inventory_diffs(Req) ->
  chromag_variant_inventory:diffs(Req).
