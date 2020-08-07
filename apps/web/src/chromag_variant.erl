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
-export([variant/2, build_variant/1, bike_size/1, color/1, price/1, id/1]).

-spec variant(web_request:url(), map()) -> #variant{}.
variant(Url, Map) ->
  F = build_variant(Url),
  F(Map).

-spec build_variant(web_request:url()) -> fun((map()) -> #variant{}).
build_variant(Url) ->
  fun(Map) ->
    #variant{
      brand = web_url:brand(Url),
      bike = web_url:bike(Url),
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
