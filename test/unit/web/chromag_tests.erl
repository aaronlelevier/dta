%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2020 8:29 AM
%%%-------------------------------------------------------------------
-module(chromag_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


filename_generated_from_request_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  Dt = "2020-07-04:0800",
  Request = chromag:create_request(Url, Dt),

  Ret = chromag:filename(Request),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/stylus-2020/2020-07-04:0800.html"),
    Ret).


dirname_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  Dt = "2020-07-04:0800",
  Request = chromag:create_request(Url, Dt),

  Ret = chromag:dirname(Request),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/stylus-2020"),
    Ret).


html_filename_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  Dt = "2020-07-04:0800",

  Ret = chromag:html_filename(Url, Dt),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/stylus-2020/2020-07-04:0800.html"),
    Ret).


bike_html_dir_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",

  Ret = chromag:bike_html_dir(Url),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/stylus-2020"),
    Ret).


meck_test() ->
  ok = meck:new(dog, [non_strict]),
  Bark = "Woof!",

  meck:expect(dog, bark, fun() -> Bark end),

  ?assertEqual(Bark, dog:bark()),
  ?assertEqual(true, meck:validate(dog)).
