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

variant_data() ->
  Dirname = filename:absname("."),
  File = "test/unit/data/variants/stylus-single.json",
  Filename = filename:join(Dirname, File),
  {ok, Bin} = file:read_file(Filename),
  jsx:decode(Bin).

url() ->
  "https://chromagbikes.com/collections/27-5-26/products/stylus-2020".

expected_variant() ->
  {variant,"chromagbikes","stylus-2020","M/L (Frame Only)",
    "CRIMSON",70900,31742775656580}.

%% Tests

variant_test() ->
  Ret = chromag_variant:variant(url(), variant_data()),

  ?assertEqual(expected_variant(), Ret).

build_variant_test() ->
  BuildVariant = chromag_variant:build_variant(url()),

  Ret = BuildVariant(variant_data()),

  ?assertEqual(expected_variant(), Ret).

color_test() ->
  ?assertEqual(
    "CRIMSON",
    chromag_variant:color(variant_data())
  ).

price_test() ->
  ?assertEqual(
    70900,
    chromag_variant:price(variant_data())
  ).

id_test() ->
  ?assertEqual(
    31742775656580,
    chromag_variant:id(variant_data())
  ).
