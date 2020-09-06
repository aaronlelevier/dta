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

%%%===================================================================
%%% API
%%%===================================================================

-spec brand(web_request:url()) -> string().
brand(Url) ->
  regex_url(Url, <<"https://([a-zA-z0-9]+)">>).

-spec bike(web_request:url()) -> string().
bike(Url) ->
  regex_url(Url, <<"/([a-zA-z0-9\-]+?)$">>).

%% @doc Returns the bike_mod based on the Url
-spec bike_mod(Url :: web_request:url()) -> atom().
bike_mod(Url) ->
  Brand = brand(Url),
  BrandMap = #{"chromagbikes" => chromag, "raawmtb" => raaw},
  maps:get(Brand, BrandMap).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec regex_url(string(), binary()) -> string().
regex_url(Url, Re) ->
  Bin = list_to_binary(Url),
  {match,[_, H2 | _]} = re:run(Bin, Re, [{capture, all, list}]),
  H2.
