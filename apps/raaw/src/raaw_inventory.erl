%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2020 5:58 AM
%%%-------------------------------------------------------------------
-module(raaw_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("dta/include/records.hrl").
-include_lib("dta/include/macros.hrl").
-export([inventory/2, inventory_diff/1,
  variant_map/1, diff_variant_map/2,
  combine_for_prev_variant_map/2, default_missing/3,
  prev_date/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec inventory(#request{}, map()) -> #inventory{}.
inventory(Req = #request{}, BikeMap) ->
  #inventory{
    variant_id = maps:get(<<"id">>, BikeMap),
    dt = Req#request.dt,
    quantity = maps:get(<<"inventory_quantity">>, BikeMap)
  }.

-spec inventory_diff(dta_types:url()) ->
  #{dta_types:variant_id() => #inventory_diff{}}.
inventory_diff(Req = #request{}) ->
  % get the last dirname dates for which we have data
  Url = Req#request.url,
  [Dt, PrevDt | _] = web_file:brand_date_dirnames(Req),
  % requests
  CurReq = web:create_request(Url, [{dt, Dt}]),
  PrevReq = web:create_request(Url, [{dt, PrevDt}]),
  % inventory maps by variant_id
  CurMap = variant_map(CurReq),
  PrevMap = variant_map(PrevReq),
  ?LOG({cur_map, CurMap}),
  ?LOG({prev_map, PrevMap}),
  % find diff
  diff_variant_map(CurMap, PrevMap).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Returns a Map where the Key is the variant_id and the value
%% is a #inventory{} record
-spec variant_map(#request{}) -> #{dta_types:variant_id() => #inventory{}}.
variant_map(Req = #request{}) ->
  BikeMaps = raaw_product_map:bike_maps(Req),
  maps:from_list([
    {maps:get(<<"id">>, X), inventory(Req, X)}
    || X <- BikeMaps
  ]).

%% @doc inventory counts NOT pre-filtered for changes in count
-spec diff_variant_map(map(), map()) ->
  #{dta_types:variant_id() => #inventory_diff{}}.
diff_variant_map(CurMap, PrevMap) ->
  CurMap2 = combine_for_prev_variant_map(PrevMap, CurMap),
  PrevMap2 = combine_for_prev_variant_map(CurMap, PrevMap),
  ?LOG({diff_variant_map, pre_zip, maps:size(CurMap)}),
  ?LOG({diff_variant_map, pre_zip, maps:size(CurMap2)}),
  ?LOG({diff_variant_map, pre_zip, maps:size(PrevMap)}),
  ?LOG({diff_variant_map, pre_zip, maps:size(PrevMap2)}),

  L = lists:zip(maps:to_list(CurMap2), maps:to_list(PrevMap2)),
  ?LOG({diff_variant_map, zipped, L}),

  maps:from_list([{Id, #inventory_diff{
    variant_id = Id,
    quantity = Cur#inventory.quantity,
    prev_quantity = Prev#inventory.quantity}} ||
    {{Id, Cur}, {Id, Prev}} <- L]).

combine_for_prev_variant_map(CurMap, PrevMap) ->
  Cur = sets:from_list(maps:keys(CurMap)),
  Prev = sets:from_list(maps:keys(PrevMap)),
  Missing = sets:to_list(sets:subtract(Cur, Prev)),
  % for all non-present VariantIds, default Prev
  % day inventory and zip (Missing)
  MissingMap = default_missing(Missing, CurMap, #{}),
  ?LOG({prev_map, PrevMap}),
  ?LOG({missing_map, MissingMap}),
  maps:merge(PrevMap, MissingMap).

%% @doc Defaults current inventory that was missing the previous day
%% to have a previous day inventory count of CurrentInventory - 1
-spec default_missing([integer()], map(), []) -> list().
default_missing([], _Cur, Acc) -> Acc;
default_missing([H | T], Cur, Acc) ->
  Inven = maps:get(H, Cur),
  Map = #{H =>
  #inventory{
    variant_id = Inven#inventory.variant_id,
    dt = prev_date(Inven#inventory.dt),
    quantity = Inven#inventory.quantity + 1
    }
  },
  ?LOG(Map),
  default_missing(T, Cur, maps:merge(Acc, Map)).

%% defaults the date string from: "2020-07-19" to "2020-07-00"
%% just so it can be counted as different from the current data
%% by zeroing out the day
prev_date(DateStr) ->
  string:join([string:slice(DateStr, 0, 8), "00"], "").
