%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Eliminators for the 'raaw' ProductMap
%%
%%% @end
%%% Created : 08. Sep 2020 8:11 AM
%%%-------------------------------------------------------------------
-module(raaw_product_map).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("dta/include/records.hrl").
-export([bike_maps/1, bike_map/2, variants/1, inventories/1]).


%%%===================================================================
%%% API
%%%===================================================================

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

-spec variants(map()) -> [map()].
variants(Map) ->
  maps:get(<<"variants">>, Map).

-spec inventories(map()) -> #{VariantId => map()} when
  VariantId :: binary().
inventories(Map) ->
  maps:from_list([{
    integer_to_binary(maps:get(<<"id">>, V)),
    #{
      <<"inventory_quantity">> =>
      bool_to_int(maps:get(<<"available">>, V))
    }
  } || V <- variants(Map)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

bool_to_int(B) ->
  case B of
    true -> 1;
    false -> 0
  end.
