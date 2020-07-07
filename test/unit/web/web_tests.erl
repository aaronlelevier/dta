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


can_crate_a_request_and_generate_a_filename_test() ->
  Url = "https://www.commencalusa.com/meta-am-29-c102x3872635",
  Request = web:create_request(Url),

  Ret = web:filename(Request),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/meta-am-29-c102x3872635.html"),
    Ret).


html_filename_test() ->
  Url = "https://www.commencalusa.com/meta-am-29-c102x3872635",

  Ret = web:html_filename(Url),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/meta-am-29-c102x3872635.html"),
    Ret).


page_name_test() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  ?assertEqual(
    "stylus-2020",
    web:page_name(Url)
  ).


html_page_name_test() ->
  ?assertEqual("stylus-2020.html", web:html_page_name("stylus-2020")).
