%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Raaw variant
%%%
%%% @end
%%% Created : 08. Sep 2020 7:52 AM
%%%-------------------------------------------------------------------
-module(raaw_variant).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("dta/include/records.hrl").
-export([variant/2, variant_map/1]).


%%%===================================================================
%%% API
%%%===================================================================

-spec variant(#request{}, map()) -> #variant{}.
variant(Req = #request{}, Map) ->
  #variant{
    id = id(Map),
    brand = web_url:brand(Req#request.url),
    bike = web_url:bike(Req#request.url),
    size = bike_size(Map),
    color = color(Map),
    price = price(Map)
  }.

%% @doc Returns a map where the Key is the variant_id and the value
%% is a #variant{} record
-spec variant_map(#request{}) -> #{dta_types:variant_id() => #variant{}}.
variant_map(Req = #request{}) ->
  BikeMaps = raaw_product_map:bike_maps(Req),
  maps:from_list([
    {maps:get(<<"id">>, X), variant(Req, X)}
    || X <- BikeMaps
  ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec bike_size(map()) -> string().
bike_size(Map) -> binary_to_list(maps:get(<<"option1">>, Map)).

-spec color(map()) -> string().
color(Map) -> binary_to_list(maps:get(<<"option2">>, Map)).

-spec price(map()) -> integer().
price(Map) -> maps:get(<<"price">>, Map).

-spec id(map()) -> integer().
id(Map) -> maps:get(<<"id">>, Map).
