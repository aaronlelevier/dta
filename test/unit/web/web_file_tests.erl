%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2020 7:50 AM
%%%-------------------------------------------------------------------
-module(web_file_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


%% Helpers
req() ->
  Url = raaw_urls:madonna_v2_frame_kit(),
  Opts = [{dt, dateutil:date_str(2020, 7, 19)}],
  F = web:build_request(raaw, Opts),
  F(Url).
%%  raaw:create_request(
%%    raaw_urls:madonna_v2_frame_kit(), [{dt, dateutil:date_str(2020, 6, 11)}]).

%% Tests
filename_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-07-19", "madonna-v2-frame-kit.html"]),
    web_file:filename(Req)
  ).

filename_empty_opts_defaults_to_html_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-07-19", "madonna-v2-frame-kit.html"]),
    web_file:filename(Req, [])
  ).


filename_opts_extension_is_json_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([
      code:priv_dir(web), "raawmtb", "2020-07-19", "madonna-v2-frame-kit.json"]),
    web_file:filename(Req, [{extension, "json"}])
  ).

dirname_test() ->
  Req = req(),
  ?assertEqual(
    filename:join([code:priv_dir(web), "raawmtb", "2020-07-19"]),
    web_file:dirname(Req)
  ).

product_map_test() ->
  Req = req(),

  Map = web_file:product_map(Req),

  ?assertEqual(true, is_map(Map)),
  ?assert(maps:size(Map) > 0).
