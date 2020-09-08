%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2020 7:55 AM
%%%-------------------------------------------------------------------
-module(raaw_variant_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

-define(VARIANT_MAP, #{<<"available">> => false,
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


variant_test() ->
  Ret = raaw_variant:variant(eunit_helpers:raaw_req(), ?VARIANT_MAP),

  ?assertEqual(
    {
      variant, 31683517972583, "raawmtb", "madonna-v2-frame-kit", "Small",
      "Raw with matt clear coat", 192437
    },
    Ret).
