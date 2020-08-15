%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2020 7:36 AM
%%%-------------------------------------------------------------------
-module(chromag_variant_inventory_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").
-import(eunit_helpers, [req/0]).

diffs_test() ->
  Ret = chromag_variant_inventory:diffs(req()),
  ?assert(is_list(Ret)),

  [H|_] = Ret,
  ?assertEqual(
    {variant_inventory_diff,31742775230596,"chromagbikes",
      "stylus-2020","S (Frame Only)","BLACK FIRE",70900,-3,-3},
    H
  ).