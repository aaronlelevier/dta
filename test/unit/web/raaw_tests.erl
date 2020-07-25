%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 9:53 AM
%%%-------------------------------------------------------------------
-module(raaw_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


%% Tests
create_request_test() ->
  Url = raaw_urls:madonna_v2_frame_kit(),

  Req = raaw:create_request(Url, [{dt, dateutil:date_str(2020, 7, 19)}]),

  ?assertEqual(
    {request, "https://raawmtb.com/collections/frames-bikes/products/madonna-v2-frame-kit",
      "2020-07-19", "raawmtb", "madonna-v2-frame-kit",
      undefined,
      {<<"id">>, <<"ProductJson-1">>}},
    Req).
