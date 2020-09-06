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
-import(eunit_helpers, [req/0, bike_map/0]).


variant_test() ->
  Ret = chromag_variant:variant(req(), bike_map()),

  ?assertEqual(
    {variant, 31742775263364, "chromagbikes", "stylus-2020", "S (Frame Only)",
      "CRIMSON", 70900},
    Ret).

variant_map_test() ->
  Ret = chromag_variant:variant_map(req()),
  ?assert(is_map(Ret)),

  [{Id, Variant} | _] = maps:to_list(Ret),
  ?assertEqual(31742775230596, Id),
  ?assertEqual(
    {variant, 31742775230596, "chromagbikes", "stylus-2020",
      "S (Frame Only)", "BLACK FIRE", 70900},
    Variant).
