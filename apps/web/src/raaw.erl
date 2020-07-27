%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  create_request/1, create_request/2, fetch_and_save/1, product_map_target/0,
  fetch_and_save_all/0, urls/0]).
-include_lib("web/include/records.hrl").


%%%%%% bike_mod behavior: start %%%%%%

urls() ->
  raaw_urls:urls().

product_map_target() -> {<<"id">>,<<"ProductJson-1">>}.

%%%%%% bike_mod behavior: end %%%%%%


%% @doc fetches and saves the product JSON for all bikes
-spec fetch_and_save_all() -> [ok].
fetch_and_save_all() ->
  [web:fetch_and_save(raaw:create_request(X)) || X <- raaw_urls:urls()].


%% @doc fetches web page and saves html of whole page and json of product map
-spec fetch_and_save(web_request:url()) -> ok.
fetch_and_save(Url) ->
  web:fetch_and_save(create_request(Url)).


%% @doc Creates a "chromag" request where the date(dt) is defaulted to today
-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% @doc Use to create request for use a specific date(dt) string
-spec create_request(web_request:url(), Opts) -> #request{} when
  Opts :: [{dt, string()} | {product_map_target, list()}].
create_request(Url, Opts) ->
  web_request:create_request(Url, [{product_map_target, product_map_target()} | Opts]).
