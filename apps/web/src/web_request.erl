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
-include_lib("web/include/records.hrl").
-export([create_request/1, create_request/2]).

%% Functions

-spec create_request(dta_types:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(dta_types:url(), Opts) -> #request{} when
  Opts :: [{dt, dta_types:dt()}].
create_request(Url, Opts) ->
  #request{
    url = Url,
    % optional arguments
    dt = proplists:get_value(dt, Opts, dateutil:date_str()),
    % computed properties
    brand = web_url:brand(Url),
    bike = web_url:bike(Url),
    product_map_target = proplists:get_value(product_map_target, Opts)
  }.

