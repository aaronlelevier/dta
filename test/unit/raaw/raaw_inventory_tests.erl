%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2020 6:16 AM
%%%-------------------------------------------------------------------
-module(raaw_inventory_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dta/include/records.hrl").

-define(BIKE_MAP, #{
  <<"available">> => false,
  <<"barcode">> => <<"4063216004749">>,
  <<"compare_at_price">> => null, <<"featured_image">> => null,
  <<"id">> => 31683517972583,
  <<"inventory_management">> => <<"shopify">>,
  <<"inventory_quantity">> => 0,
  <<"name">> =>
  <<"Madonna V2 Frame Kit - Small / Raw with matt clear coat">>,
  <<"option1">> => <<"Small">>,
  <<"option2">> => <<"Raw with matt clear coat">>,
  <<"option3">> => null,
  <<"options">> =>
  [<<"Small">>, <<"Raw with matt clear coat">>],
  <<"price">> => 192437,
  <<"public_title">> => <<"Small / Raw with matt clear coat">>,
  <<"requires_shipping">> => true,
  <<"sku">> => <<"19RA0202SM1">>, <<"taxable">> => true,
  <<"title">> => <<"Small / Raw with matt clear coat">>,
  <<"weight">> => 3600}).

inventory_test() ->
  Ret = raaw_inventory:inventory(eunit_helpers:raaw_req(), ?BIKE_MAP),
  ?assertEqual(
    {inventory, 31683517972583, "2020-07-19", 0},
    Ret).

inventory_diff_test() ->
  Url = eunit_helpers:raaw_url(),
  Req = web:create_request(Url, [{dt, dateutil:date_str(2020, 9, 9)}]),

  Ret = raaw_inventory:inventory_diff(Req),

  ?assert(is_map(Ret)),
  ?assert(maps:size(Ret) > 0).

diff_variant_map_test() ->
  PrevDate = "2020-09-08",
  CurDate = "2020-09-09",
  PrevMap = #{
    1 => #inventory{variant_id = 1, dt = PrevDate, quantity = 3}
  },
  CurMap = #{
    1 => #inventory{variant_id = 1, dt = CurDate, quantity = 4},
    2 => #inventory{variant_id = 2, dt = CurDate, quantity = 2}
  },

  Ret = raaw_inventory:diff_variant_map(CurMap, PrevMap),

  ?assertEqual(
    #{1 => {inventory_diff, 1, 4, 3}, 2 => {inventory_diff, 2, 2, 3}},
    Ret
  ).

combine_variant_map_test() ->
  PrevDate = "2020-09-08",
  CurDate = "2020-09-09",
  PrevMap = #{
    1 => #inventory{variant_id = 1, dt = PrevDate, quantity = 3}
  },
  CurMap = #{
    1 => #inventory{variant_id = 1, dt = CurDate, quantity = 4},
    2 => #inventory{variant_id = 2, dt = CurDate, quantity = 2}
  },

  Ret = raaw_inventory:combine_for_prev_variant_map(
    CurMap, PrevMap),

  ?assertEqual(
    #{
      1 => #inventory{variant_id = 1, dt = "2020-09-08", quantity = 3},
      2 => #inventory{variant_id = 2, dt = "2020-09-00", quantity = 3}
    },
    Ret
  ).

default_missing_test() ->
  Missing = [1],
  CurDate = "2020-09-09",
  Cur = #{
    1 => #inventory{variant_id = 1, dt = CurDate, quantity = 0},
    2 => #inventory{variant_id = 2, dt = CurDate, quantity = 0}
  },

  Ret = raaw_inventory:default_missing(Missing, Cur, #{}),

  ?assertEqual(
    #{1 => {inventory, 1, raaw_inventory:prev_date(CurDate), 1}},
    Ret
  ).

prev_date_test() ->
  S = "2020-09-09",
  Ret = raaw_inventory:prev_date(S),
  ?assertEqual("2020-09-00", Ret).
