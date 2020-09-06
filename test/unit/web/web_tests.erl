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
-import(eunit_helpers, [url/0, raaw_req/0, raaw_url/0]).

create_request_chromag_test() ->
  Dt = dateutil:date_str(),

  Req = web:create_request(url()),

  ?assertEqual(
    {request, url(), Dt, "chromagbikes", "stylus-2020",
      {<<"data-product-json">>, <<"data-product-json">>}},
    Req).

create_request_with_opts_raaw_test() ->
  Req = web:create_request(raaw_url(), [{dt, dateutil:date_str(2020, 7, 19)}]),

  ?assertEqual(
    {request, raaw_url(), "2020-07-19", "raawmtb", "madonna-v2-frame-kit",
      {<<"id">>, <<"ProductJson-1">>}},
    Req).

fetch_single_test() ->
  Url = raaw_url(),
  Req = web:create_request(Url, [{dt, dateutil:date_str(2020, 1, 1)}]),
  Filename = web_file:filename(Req, [{extension, "json"}]),
  %% mock setup
  {ok, Bin} = web_file:file_read(raaw_req()),
  Expected = binary_to_list(Bin),
  ?assertEqual(ok, meck:new(httpc)),
  ?assertEqual(ok, meck:expect(
    httpc,
    request,
    fun(_A, _B, _C, _D) ->
      {ok, {status, headers, Expected}}
    end)),
  %% no JSON file present
  ?assertEqual(ok, web_file:file_delete(Filename)),
  ?assertEqual(false, filelib:is_regular(Filename)),

  %% we fetch a single page
  Ret = web:fetch_single(Req),
  ?assertEqual(ok, Ret),

  %% now the JSON file exists
  ?assertEqual(true, filelib:is_regular(Filename)),
  %% mock
  ?assertEqual(true, meck:validate(httpc)),
  ?assertEqual(ok, meck:unload(httpc)),
  %% cleanup
  ?assertEqual(ok, web_file:file_delete(Filename)).

fetch_page_test() ->
  Req = raaw_req(),
  {ok, Bin} = web_file:file_read(Req),
  Expected = binary_to_list(Bin),
  % mock setup
  ok = meck:expect(
    httpc,
    request,
    fun(_A, _B, _C, _D) ->
      {ok, {status, headers, Expected}}
    end),

  {ok, Ret} = web:fetch_page(raaw_url()),

  ?assert(is_list(Expected)),
  ?assert(is_list(Ret)),
  [A, B, C | _] = Expected,
  [D, E, F | _] = Ret,
  ?assertEqual([D, E, F], [A, B, C]),
  % mock asserts
  ?assertEqual(true, meck:validate(httpc)),
  ?assertEqual(ok, meck:unload(httpc)).
