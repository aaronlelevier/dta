%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2020 7:11 AM
%%%-------------------------------------------------------------------
-module(chromag2_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

create_request_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",

  Req = chromag2:create_request(Url, [{dt, dateutil:date_str(2020, 7, 19)}]),

  ?assertEqual(
    {request, Url,
      "2020-07-19", "chromagbikes", "stylus-2020",
      undefined,
      {<<"data-product-json">>, <<"data-product-json">>}},
    Req).
