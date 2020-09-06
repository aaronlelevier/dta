%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Module for interacting with 'inventory' and 'inventory_diff' records
%%%
%%% @end
%%% Created : 08. Aug 2020 8:04 AM
%%%-------------------------------------------------------------------
-module(chromag_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-export([inventory/2, inventory_diff/1]).
-include_lib("web/include/records.hrl").

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
  #{web_types:variant_id() => #inventory_diff{}}.
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
-spec variant_map(#request{}) -> #{web_types:variant_id() => #inventory{}}.
variant_map(Req = #request{}) ->
  BikeMaps = chromag_product_map:bike_maps(Req),
  maps:from_list([
    {maps:get(<<"id">>, X), inventory(Req, X)}
    || X <- BikeMaps
  ]).

%% @doc inventory counts NOT pre-filtered for changes in count
-spec diff_variant_map(map(), map()) ->
  #{web_types:variant_id() => #inventory_diff{}}.
diff_variant_map(CurMap, PrevMap) ->
  L = lists:zip(maps:to_list(CurMap), maps:to_list(PrevMap)),
  maps:from_list([{Id, #inventory_diff{
    variant_id = Id,
    quantity = Cur#inventory.quantity,
    prev_quantity = Prev#inventory.quantity}} ||
    {{Id, Cur}, {Id, Prev}} <- L]).
