%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc "request" record type call back module
%%%
%%% @end
%%% Created : 07. Jul 2020 7:06 AM
%%%-------------------------------------------------------------------
-module(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-export([create_request/1, create_request/2, bike_mod/1]).
-include_lib("dta/include/records.hrl").
-include_lib("dta/include/macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Constructors

-spec create_request(dta_types:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(dta_types:url(), Opts) -> #request{} when
  Opts :: [{dt, dta_types:dt()}].
create_request(Url, Opts) ->
  Brand = web_url:brand(Url),
  BikeMod = bike_mod(Brand),
  #request{
    url = Url,
    % optional arguments
    dt = proplists:get_value(dt, Opts, dateutil:date_str()),
    % computed properties
    brand = Brand,
    bike = web_url:bike(Url),
    product_map_target = BikeMod:product_map_target()
  }.

%% Eliminators

-spec bike_mod(#request{} | string()) -> atom().
bike_mod(Req = #request{}) ->
  maps:get(Req#request.brand, ?BRAND_MAP);
bike_mod(Brand) when is_list(Brand) ->
  maps:get(Brand, ?BRAND_MAP).

%%%===================================================================
%%% Internal functions
%%%===================================================================
