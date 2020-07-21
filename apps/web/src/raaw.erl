%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-behavior(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  create_request/1, create_request/2, filename/1, filename/2,
  dirname/1, file_read/1, file_write/2, product_map/1, fetch_and_save/1,
  product_map_target/0]).
-include_lib("web/include/records.hrl").


%% @doc The target tag for the product map
product_map_target() -> {<<"id">>,<<"ProductJson-1">>}.

-spec fetch_and_save(web_request:url()) -> ok.
fetch_and_save(Url) ->
  {ok, Body} = web:fetch_page(Url),
  Req = create_request(Url),
  % save raw file
  ok = file_write(Req, Body),
  % QUESTION: could save the product JSON per day or convert later?
  Map = product_map(Req),
  ok = file:write_file(
    raaw:filename(Req, [{extension, "json"}]),
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.

%% request - constructors

-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(web_request:url(), Opts) -> #request{} when
  Opts :: [{dt, string()} | {product_info_location, list()}].
create_request(Url, Opts) ->
  web:create_request(Url, [{product_map_target, product_map_target()} | Opts]).


%% file funcs

-spec filename(#request{}) -> string().
filename(Req = #request{}) ->
  filename:join(
    dirname(Req),
    string:concat(Req#request.bike, ".html")
  ).

-spec filename(#request{}, Opts) -> string() when
  Opts :: [{extension, string()}].
filename(Req = #request{}, Opts) ->
  Ext = proplists:get_value(extension, Opts, "html"),
  filename:join(
    dirname(Req),
    string:concat(Req#request.bike, string:concat(".", Ext))
  ).

-spec dirname(#request{}) -> string().
dirname(Req = #request{}) ->
  filename:join([
    code:priv_dir(web),
    Req#request.brand,
    Req#request.dt
  ]).

-spec file_write(#request{}, [byte()]) -> ok | {error, string()}.
file_write(Req = #request{}, Bytes) ->
  ok = fileutil:make_dir(dirname(Req)),
  ok = file:write_file(filename(Req), Bytes),
  ok.

-spec file_read(#request{}) -> {ok, binary()} | {error, string()}.
file_read(Req = #request{}) ->
  file:read_file(raaw:filename(Req)).

%% product map of JSON

-spec product_map(#request{}) -> map().
product_map(Req = #request{}) ->
  {ok, Bin} = file_read(Req),
  Tree = mochiweb_html:parse(Bin),
  [Bin2|_] = web_html:findsingle(Tree, Req#request.product_map_target),
  jsx:decode(Bin2).
