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

%% Helpers

req() ->
  F = web:build_request(chromag, [{dt, dateutil:date_str(2020, 8, 8)}]),
  Req = F("https://chromagbikes.com/collections/27-5-26/products/stylus-2020"),
  Req.

bike_map() ->
  [H | _] = chromag_product_map:bike_maps(req()),
  H.

%% Tests

inventory_test() ->
  Ret = chromag_inventory:inventory(req(), bike_map()),

  ?assertEqual(Ret, {inventory, 31742775263364, "2020-08-08", -1}).
