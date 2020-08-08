%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2020 8:05 AM
%%%-------------------------------------------------------------------
-module(chromag_variant_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


%% Helpers

url() ->
  "https://chromagbikes.com/collections/27-5-26/products/stylus-2020".

req() ->
  F = web:build_request(chromag, [{dt, dateutil:date_str(2020, 8, 8)}]),
  Req = F(url()),
  Req.

expected_variant() ->
  {variant,"chromagbikes","stylus-2020","S (Frame Only)",
    "CRIMSON",70900,31742775263364}.

bike_map() ->
  [H | _] = chromag_product_map:bike_maps(req()),
  H.

%% Tests

variant_test() ->
  Ret = chromag_variant:variant(req(), bike_map()),

  ?assertEqual(expected_variant(), Ret).

build_variant_test() ->
  BuildVariant = chromag_variant:build_variant(req()),

  Ret = BuildVariant(bike_map()),

  ?assertEqual(expected_variant(), Ret).

color_test() ->
  ?assertEqual(
    "CRIMSON",
    chromag_variant:color(bike_map())
  ).

price_test() ->
  ?assertEqual(
    70900,
    chromag_variant:price(bike_map())
  ).

id_test() ->
  ?assertEqual(
    31742775263364,
    chromag_variant:id(bike_map())
  ).
