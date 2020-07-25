%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 8:06 AM
%%%-------------------------------------------------------------------
-module(chromag2).
-author("Aaron Lelevier").
-vsn(1.0).
-export([fetch_and_save/1, product_map_target/0, create_request/1, create_request/2]).
-include_lib("web/include/records.hrl").


-spec fetch_and_save(web_request:url()) -> ok.
fetch_and_save(Url) ->
  % this needs to be bike specific
  Req = create_request(Url),
  % impl by "web_file" and dynamic per the "Req" value
  {ok, Body} = web:fetch_page(Req#request.url),
  ok = web_file:file_write(Req, Body),
  Map = web_file:product_map(Req),
  ok = web_file:file_write_product_map(Req, Map),
  ok.

%% @doc The target tag for the product map
product_map_target() -> {<<"data-product-json">>, <<"data-product-json">>}.

%% @doc Creates a "chromag" request where the date(dt) is defaulted to today
-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% @doc Use to create request for use a specific date(dt) string
-spec create_request(web_request:url(), Opts) -> #request{} when
  Opts :: [{dt, string()} | {product_info_location, list()}].
create_request(Url, Opts) ->
  web:create_request(Url, [{product_map_target, product_map_target()} | Opts]).

