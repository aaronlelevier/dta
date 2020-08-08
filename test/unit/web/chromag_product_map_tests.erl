%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 9:24 AM
%%%-------------------------------------------------------------------
-module(chromag_product_map_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

%% Helpers

req() ->
  F = web:build_request(chromag, [{dt, dateutil:date_str(2020, 8, 8)}]),
  F("https://chromagbikes.com/collections/27-5-26/products/stylus-2020").

product_map() ->
  web_file:product_map(req(), [{extension, "json"}]).

%% Tests

bike_maps_test() ->
  Ret = chromag_product_map:bike_maps(req()),
  ?assert(is_list(Ret)),
  ?assert(length(Ret) > 0),

  [H|_] = Ret,
  ?assert(is_map(H)).

product_map_test() ->
  Ret = product_map(),
  ?assert(is_map(Ret)).

bike_map_test() ->
  Map = product_map(),
  [VariantMap|_] = chromag_product_map:variants(Map),
  Inventories = chromag_product_map:inventories(Map),

  Ret = chromag_product_map:bike_map(VariantMap, Inventories),

  ?assert(is_map(Ret)).

variants_test() ->
  Map = product_map(),

  Ret = chromag_product_map:variants(Map),

  ?assert(is_list(Ret)),
  ?assert(length(Ret) > 0),

  [H|_] = Ret,
  % each variant is a non-empty map
  ?assert(is_map(H)),
  ?assert(maps:size(H) > 0).

inventories_test() ->
  Map = product_map(),

  Ret = chromag_product_map:inventories(Map),

  ?assert(is_map(Ret)),
  ?assert(maps:size(Ret) > 0).
