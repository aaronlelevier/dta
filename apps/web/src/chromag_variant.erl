%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag_variant).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").
-export([variant/2, build_variant/1, bike_size/1, color/1, price/1, id/1,
  variant_map/1]).

-spec variant(#request{}, map()) -> #variant{}.
variant(Req = #request{}, Map) ->
  F = build_variant(Req),
  F(Map).

-spec build_variant(#request{}) -> fun((map()) -> #variant{}).
build_variant(Req = #request{}) ->
  fun(Map) ->
    #variant{
      brand = web_url:brand(Req#request.url),
      bike = web_url:bike(Req#request.url),
      size = bike_size(Map),
      color = color(Map),
      price = price(Map),
      id = id(Map)
    }
  end.

-spec bike_size(map()) -> string().
bike_size(Map) -> binary_to_list(maps:get(<<"option2">>, Map)).

-spec color(map()) -> string().
color(Map) -> binary_to_list(maps:get(<<"option1">>, Map)).

-spec price(map()) -> integer().
price(Map) -> maps:get(<<"price">>, Map).

-spec id(map()) -> integer().
id(Map) -> maps:get(<<"id">>, Map).

%% @doc Returns a map where the Key is the variant_id and the value
%% is a #variant{} record
-spec variant_map(#request{}) -> #{web_types:variant_id() => #variant{}}.
variant_map(Req = #request{}) ->
  BikeMaps = chromag_product_map:bike_maps(Req),
  maps:from_list([
    {maps:get(<<"id">>, X), variant(Req, X)}
    || X <- BikeMaps
  ]).
