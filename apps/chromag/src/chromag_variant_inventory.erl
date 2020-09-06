%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Variant and Inventory combined information
%%%
%%% @end
%%% Created : 14. Aug 2020 11:23 AM
%%%-------------------------------------------------------------------
-module(chromag_variant_inventory).
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
  InventoryDiffs = chromag_inventory:inventory_diff(Req),
  Variants = chromag_variant:variant_map(Req),
  [diff(maps:get(Id, Variants), InventoryDiff) ||
    {Id, InventoryDiff} <- maps:to_list(InventoryDiffs)].

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
