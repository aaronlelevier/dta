%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 10:10 AM
%%%-------------------------------------------------------------------
-module(chromag_inventory_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").
-import(eunit_helpers, [req/0, bike_map/0]).


inventory_test() ->
  Ret = chromag_inventory:inventory(req(), bike_map()),

  ?assertEqual({inventory, 31742775263364, "2020-08-08", -1}, Ret).

inventory_diff_test() ->
  Ret = chromag_inventory:inventory_diff(req()),
  ?assert(is_map(Ret)),

  [{Id, InventoryDiff} | _] = maps:to_list(Ret),
  ?assertEqual(31742775230596, Id),
  ?assertEqual({inventory_diff, 31742775230596, -3, -3}, InventoryDiff).
