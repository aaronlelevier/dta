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

%% Helpers
raaw_url() ->
  "https://raawmtb.com/collections/frames-bikes/products/madonna-v2-frame-kit".

%% Tests
filename_test() ->
  Req = raaw:create_request(raaw_url(), dateutil:date_str(2020, 6, 11)),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-06-11", "madonna-v2-frame-kit.html"]),
    raaw:filename(Req)
  ).

dirname_test() ->
  Req = raaw:create_request(raaw_url(), dateutil:date_str(2020, 6, 11)),
  ?assertEqual(
    filename:join([code:priv_dir(web), "raawmtb", "2020-06-11"]),
    raaw:dirname(Req)
  ).
