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

create_request_test() ->
  Url = raaw_urls:madonna_v2_frame_kit(),

  Req = web_request:create_request(Url, [{dt, dateutil:date_str(2020, 7, 19)}]),

  ?assertEqual(
    {request, Url,
      "2020-07-19", "raawmtb", "madonna-v2-frame-kit", undefined},
    Req).
