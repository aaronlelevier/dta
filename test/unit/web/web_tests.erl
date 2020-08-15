%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2020 11:46 AM
%%%-------------------------------------------------------------------
-module(web_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").
-import(eunit_helpers, [url/0]).

create_request_chromag_test() ->
  Dt = dateutil:date_str(),

  Req = web:create_request(url()),

  ?assertEqual(
    {request, url(), Dt, "chromagbikes", "stylus-2020",
      {<<"data-product-json">>, <<"data-product-json">>}},
    Req).

create_request_with_opts_raaw_test() ->
  Url = raaw_urls:madonna_v2_frame_kit(),

  Req = web:create_request(Url, [{dt, dateutil:date_str(2020, 7, 19)}]),

  ?assertEqual(
    {request, Url, "2020-07-19", "raawmtb", "madonna-v2-frame-kit",
      {<<"id">>, <<"ProductJson-1">>}},
    Req).
