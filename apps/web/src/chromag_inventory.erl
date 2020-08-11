%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 8:04 AM
%%%-------------------------------------------------------------------
-module(chromag_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-export([variant_map/1, inventory_diff/1, diff_variant_map/2, inventory/2, build_inventory/1]).
-include_lib("web/include/records.hrl").

%% Types

-type cur_quantity() :: integer().
-type prev_quantity() :: integer().
-type inventory_diff() :: [{web_types:variant_id(), cur_quantity(), prev_quantity()}].

%% Functions

-spec inventory(#request{}, map()) -> #inventory{}.
inventory(Req = #request{}, Map) ->
  F = build_inventory(Req),
  F(Map).

-spec build_inventory(#request{}) -> fun((map()) -> #inventory{}).
build_inventory(Req = #request{}) ->
  fun(Map) ->
    #inventory{
      variant_id = maps:get(<<"id">>, Map),
      dt = Req#request.dt,
      quantity = maps:get(<<"inventory_quantity">>, Map)
    }
  end.

%% @doc Returns a Map where the Key is the variant_id and the value
%% is a #inventory{} record
-spec variant_map(#request{}) -> #{web_types:variant_id() => #inventory{}}.
variant_map(Req = #request{}) ->
  BikeMaps = chromag_product_map:bike_maps(Req),
  maps:from_list([
    {maps:get(<<"id">>, X), inventory(Req, X)}
    || X <- BikeMaps
  ]).

-spec inventory_diff(web_request:url()) -> inventory_diff().
inventory_diff(Url) ->
  % get the last dirname dates for which we have data
  Req = web_request:create_request(Url),
  [Dt, PrevDt | _] = web_file:brand_date_dirnames(Req),
  % requests
  CurReq = web:create_request(Url, [{dt, Dt}]),
  PrevReq = web:create_request(Url, [{dt, PrevDt}]),
  % inventory maps by variant_id
  CurMap = variant_map(CurReq),
  PrevMap = variant_map(PrevReq),
  % find diff
  diff_variant_map(CurMap, PrevMap).

-spec diff_variant_map(map(), map()) -> inventory_diff().
diff_variant_map(CurMap, PrevMap) ->
  L = lists:zip(maps:to_list(CurMap), maps:to_list(PrevMap)),
  [{Id, Cur#inventory.quantity, Prev#inventory.quantity} ||
    {{Id, Cur}, {Id, Prev}} <- L,
    Cur#inventory.quantity /= Prev#inventory.quantity].