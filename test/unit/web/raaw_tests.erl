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


req() ->
  raaw:create_request(
    raaw_urls:madonna_v2_frame_kit(), [{dt, dateutil:date_str(2020, 6, 11)}]).

%% Tests
filename_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-06-11", "madonna-v2-frame-kit.html"]),
    raaw:filename(Req)
  ).

filename_empty_opts_defaults_to_html_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-06-11", "madonna-v2-frame-kit.html"]),
    raaw:filename(Req, [])
  ).


filename_opts_extension_is_json_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-06-11", "madonna-v2-frame-kit.json"]),
    raaw:filename(Req, [{extension, "json"}])
  ).

dirname_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([code:priv_dir(web), "raawmtb", "2020-06-11"]),
    raaw:dirname(Req)
  ).

product_map_test() ->
  Url = raaw_urls:madonna_v2_frame_kit(),
  Req = raaw:create_request(Url, dateutil:date_str(2020, 7, 19)),

  Map = raaw:product_map(Req),

  ?assertEqual(true, is_map(Map)),
  ?assert(maps:size(Map) > 0).

