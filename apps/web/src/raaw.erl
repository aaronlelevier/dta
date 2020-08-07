%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc raaw impl of bike_mod - only contains this interface
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, product_map_target/0]).

urls() ->
  raaw_urls:urls().

product_map_target() -> {<<"id">>, <<"ProductJson-1">>}.
