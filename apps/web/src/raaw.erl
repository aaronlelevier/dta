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
  dirname/1, file_read/1, file_write/2, product_map/1, fetch_and_save/1]).
-include_lib("web/include/records.hrl").


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
  create_request(Url, dateutil:date_str()).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(web_request:url(), string()) -> #request{}.
create_request(Url, Dt) ->
  #request{
    url = Url,
    dt = Dt,
    brand = web_url:brand(Url),
    bike = web_url:bike(Url)
  }.

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
  L = web:findall([<<"html">>, <<"body">>, <<"div">>, <<"div">>, <<"script">>], Tree),
  Bin2 = web_html:extract_content(L),
  jsx:decode(Bin2).
