%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Eliminator functions for Urls
%%%
%%% @end
%%% Created : 11. Jul 2020 8:50 AM
%%%-------------------------------------------------------------------
-module(web_url).
-author("Aaron Lelevier").
-vsn(1.0).
-export([brand/1, bike/1, bike_mod/1]).
-include_lib("dta/include/macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec brand(dta_types:url()) -> string().
brand(Url) ->
  regex_url(Url, <<"https://([a-zA-z0-9]+)">>).

-spec bike(dta_types:url()) -> string().
bike(Url) ->
  regex_url(Url, <<"/([a-zA-z0-9\-]+?)$">>).

%% @doc Returns the bike_mod based on the Url
-spec bike_mod(Url :: dta_types:url()) -> atom().
bike_mod(Url) ->
  Brand = brand(Url),
  maps:get(Brand, ?BRAND_MAP).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec regex_url(string(), binary()) -> string().
regex_url(Url, Re) ->
  Bin = list_to_binary(Url),
  {match,[_, H2 | _]} = re:run(Bin, Re, [{capture, all, list}]),
  H2.
