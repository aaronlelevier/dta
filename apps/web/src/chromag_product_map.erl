%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Creates 'variant' and 'inventory' records from the product map
%%%
%%% @end
%%% Created : 08. Aug 2020 8:17 AM
%%%-------------------------------------------------------------------
-module(chromag_product_map).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").
-export([bike_maps/1, bike_map/2, variants/1, inventories/1]).


%% @doc combined bike maps for all versions of a single type brand/bike
-spec bike_maps(#request{}) -> [map()].
bike_maps(Req = #request{}) ->
  Map = web_file:product_map(Req, [{extension, "json"}]),
  Variants = variants(Map),
  Inventories = inventories(Map),
  [bike_map(VariantMap, Inventories) || VariantMap <- Variants].

%% @doc The combined variant and inventory map data for a single bike
%% derived from the 'product_map
bike_map(Hv, Inventories) ->
  VariantId = maps:get(<<"id">>, Hv),
  Hi = maps:get(integer_to_binary(VariantId), Inventories),
  maps:merge(Hv, Hi).

variants(Map) ->
  maps:get(<<"variants">>, maps:get(<<"product">>, Map)).

inventories(Map) ->
  maps:get(<<"inventories">>, Map).
