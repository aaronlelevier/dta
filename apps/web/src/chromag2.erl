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
  {ok, Body} = web:fetch_page(Url),
  %%  Req = raaw:create_request(
  %%    Url, [{product_info_location, [<<"html">>, <<"body">>, <<"div">>, <<"main">>, <<"div">>, <<"section">>,
  %%      <<"div">>, <<"div">>, <<"div">>, <<"div">>, <<"script">>]}]),
  Req = create_request(Url),

  % save raw file
  ok = raaw:file_write(Req, Body),
  % QUESTION: could save the product JSON per day or convert later?
  Map = raaw:product_map(Req),

  %% TODO: could be a helper function
  ok = file:write_file(
    raaw:filename(Req, [{extension, "json"}]),
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.

-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(web_request:url(), Opts) -> #request{} when
  Opts :: [{dt, string()} | {product_info_location, list()}].
create_request(Url, Opts) ->
  web:create_request(Url, [{product_map_target, product_map_target()} | Opts]).


%% @doc The target tag for the product map
%% if a tag is a key only, it's value is the same as it's key in 'mochiweb_html'
product_map_target() -> {<<"data-product-json">>, <<"data-product-json">>}.
