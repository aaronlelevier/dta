%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:50 AM
%%%-------------------------------------------------------------------
-module(web_url).
-author("Aaron Lelevier").
-vsn(1.0).
-export([brand/1, bike/1]).


%% Public
-spec brand(web_request:url()) -> string().
brand(Url) ->
  regex_url(Url, <<"https://([a-zA-z0-9]+)">>).


-spec bike(web_request:url()) -> string().
bike(Url) ->
  regex_url(Url, <<"/([a-zA-z0-9\-]+?)$">>).


%% Private
-spec regex_url(string(), binary()) -> string().
regex_url(Url, Re) ->
  Bin = list_to_binary(Url),
  {match,[_, H2 | _]} = re:run(Bin, Re, [{capture, all, list}]),
  H2.
