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

%%diff_variant_map_test() ->
%%  CurMap = #{
%%    1 => #inventory{quantity = 0},
%%    2 => #inventory{quantity = 0}
%%  },
%%  PrevMap = #{
%%    2 => #inventory{quantity = 1},
%%    3 => #inventory{quantity = 0}
%%  },
%%
%%  Ret = raaw_inventory:combine_variant_map(CurMap, PrevMap),
%%
%%  ?assertEqual(
%%    [
%%      {{1, {inventory, 0}}, {1, {inventory, -1}}},
%%      {{2, {inventory, 0}}, {2, {inventory, 1}}}
%%    ],
%%    Ret
%%  ).

default_missing_test() ->
  Missing = [2],
  Cur = #{
    1 => #inventory{variant_id = 1, dt = "2020-09-09", quantity = 0},
    2 => #inventory{variant_id = 2, dt = "2020-09-09", quantity = 0}
  },

  Ret = raaw_inventory:default_missing(Missing, Cur, []),

  ?assertEqual(
    [
      {
        {2, {inventory, 2, "2020-09-09", 0}},
        {2, {inventory, 2, "2020-09-08", -1}}
      }
    ],
    Ret
  ).