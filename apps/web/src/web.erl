%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2020 10:59 AM
%%%-------------------------------------------------------------------
-module(web).
-author("Aaron Lelevier").
-vsn(1.0).
-export([create_request/1, create_request/2, fetch_page/1, fetch_single/1]).
-include_lib("dta/include/macros.hrl").
-include_lib("web/include/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

create_request(Url) ->
  create_request(Url, []).

create_request(Url, Opts) ->
  BikeMod = web_url:bike_mod(Url),
  web_request:create_request(
    Url, [{product_map_target, BikeMod:product_map_target()} | Opts]).


%%%% @doc Fetches and saves a single bike/brand's product JSON and saves
%%%% it to a file
%%-spec fetch_single(web_request:url()) -> ok.

fetch_single(Url) when is_list(Url) ->
  fetch_single(create_request(Url));
fetch_single(Req = #request{}) ->
  {ok, Body} = fetch_page(Req#request.url),
  ok = web_file:file_write(Req, Body),
  Map = web_file:product_map(Req),
  ok = web_file:file_write_product_map(Req, Map),
  % remove the HTML file after JSON successfully saved
  ok = file:delete(web_file:filename(Req)),
  ok.


%% @doc fetch a HTML page and caches it to the "priv/html" dir
%% returns binary() of page contents
-spec fetch_page(Url :: string()) -> {ok, Bytes} when
  Bytes :: [byte()].
fetch_page(Url) ->
  % fetch page
  ?LOG({url, Url}),
  ReqHeaders = [{"User-Agent", "dta"}],
  {ok, {_Status, _Headers, Body}} = httpc:request(get, {Url, ReqHeaders}, [], []),
  {ok, Body}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
