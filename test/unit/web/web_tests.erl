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

html_filename_test() ->
  Url = "https://www.commencalusa.com/meta-am-29-c102x3872635",

  Ret = web:html_filename(Url),

  ?assertEqual(
    filename:join(code:priv_dir(web), "html/meta-am-29-c102x3872635.html"),
    Ret).

