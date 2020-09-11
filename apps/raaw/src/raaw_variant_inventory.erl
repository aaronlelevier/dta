%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2020 7:05 AM
%%%-------------------------------------------------------------------
-module(raaw_variant_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-export([diffs/1]).
-include_lib("dta/include/records.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @doc All diffs for a single 'request'
-spec diffs(#request{}) -> [#variant_inventory_diff{}].
diffs(Req = #request{}) ->
  %% TODO: 'diffs/1' are not BikeMod specific, so the below 2 func calls shouldn't be either
  InventoryDiffs = raaw_inventory:inventory_diff(Req),
  Variants = raaw_variant:variant_map(Req),
  [diff(maps:get(Id, Variants), InventoryDiff) ||
    {Id, InventoryDiff} <- maps:to_list(InventoryDiffs),
    maps:is_key(Id, Variants) =:= true].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec diff(#variant{}, #inventory_diff{}) -> #variant_inventory_diff{}.
diff(Variant = #variant{}, InventoryDiff = #inventory_diff{}) ->
  Proplist = lists:merge(
    recordutil:record_to_proplist(Variant),
    recordutil:record_to_proplist(InventoryDiff)
  ),
  recordutil:proplist_to_record(Proplist, variant_inventory_diff).
