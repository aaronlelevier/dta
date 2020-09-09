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
-export([inventory/2, inventory_diff/1]).
-include_lib("dta/include/records.hrl").
-include_lib("dta/include/macros.hrl").

-compile(export_all).

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
  %% TODO: assumes the maps are of the same size with the same keys
  %% TODO: i.e. the variant_id's haven't changed
  L = lists:zip(maps:to_list(CurMap), maps:to_list(PrevMap)),
  maps:from_list([{Id, #inventory_diff{
    variant_id = Id,
    quantity = Cur#inventory.quantity,
    prev_quantity = Prev#inventory.quantity}} ||
    {{Id, Cur}, {Id, Prev}} <- L]).

%% TODO: should move to shared map-inventory util
combine_variant_map(CurMap, PrevMap) ->
  Cur = sets:from_list(maps:keys(CurMap)),
  Prev = sets:from_list(maps:keys(PrevMap)),
  Union = sets:union(Cur, Prev),
  Subtract = sets:subtract(Cur, Prev),

  % for all non-present VariantIds, default Prev day inventory and zip (Subtract)
  L = default_missing(sets:to_list(Subtract), CurMap, []),

  % for those present in both, just zip (Union)
  L2 = present_in_both(Union, Cur, Prev),

  lists:join(L, L2).

%% @doc Defaults current inventory that was missing the previous day
%% to have a previous day inventory count of CurrentInventory - 1
-spec default_missing([integer()], map(), []) -> list().
default_missing([], Cur, Acc) ->
  A = [{X,Y} ||
    {X,Y} <- maps:to_list(Cur),
    lists:member(X, [J || {J,_} <- Acc]) == true],
  B = lists:reverse(Acc),
  ?LOG(A),
  ?LOG(B),
  lists:zip(A, B);
default_missing([H | T], Cur, Acc) ->
  Inven = maps:get(H, Cur),
  Map = {H,
  #inventory{
    variant_id = Inven#inventory.variant_id,
    dt = prev_date(Inven#inventory.dt),
    quantity = Inven#inventory.quantity - 1
    }
  },
  ?LOG(Map),
  default_missing(T, Cur, [Map|Acc]).

%% TODO: impl next
present_in_both(Union, Cur, Prev) -> 0.

%% TODO: need to date math of -1 on "X"
prev_date(_X) -> "2020-09-08".