%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc chromag impl of bike_mod - only contains this interface
%%%
%%% @end
%%% Created : 02. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, product_map_target/0]).

urls() ->
  chromag_urls:urls().

product_map_target() -> {<<"data-product-json">>, <<"data-product-json">>}.
