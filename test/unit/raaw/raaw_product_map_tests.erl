%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2020 8:16 AM
%%%-------------------------------------------------------------------
-module(raaw_product_map_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

-define(VARIANT_MAP, #{
  <<"available">> => false,
  <<"barcode">> => <<"4063216004749">>,
  <<"compare_at_price">> => null, <<"featured_image">> => null,
  <<"id">> => 31683517972583,
  <<"inventory_management">> => <<"shopify">>,
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

variants_test() ->
  Req = eunit_helpers:raaw_req(),
  Map = web_file:product_map(Req, [{extension, "json"}]),

  Ret = raaw_product_map:variants(Map),
  [Variant | _] = Ret,

  ?assertEqual(
    ?VARIANT_MAP,
    Variant
  ).

inventories_test() ->
  Req = eunit_helpers:raaw_req(),
  Map = web_file:product_map(Req, [{extension, "json"}]),

  Ret = raaw_product_map:inventories(Map),

  L = maps:to_list(Ret),
  [H|_] = L,
  ?assertEqual(
    {<<"31683517972583">>, #{<<"inventory_quantity">> => 0}},
    H
  ).
