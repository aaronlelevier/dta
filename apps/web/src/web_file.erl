%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc File I/O functions and ProductMap helpers
%%%
%%% @end
%%% Created : 25. Jul 2020 7:22 AM
%%%-------------------------------------------------------------------
-module(web_file).
-behavior(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-export([filename/1, filename/2, dirname/1, file_read/1, file_write/2,
  product_map/1, file_write_product_map/2]).
-include_lib("web/include/records.hrl").

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
  file:read_file(filename(Req)).

%% product map of JSON functions %%

-spec product_map(#request{}) -> map().
product_map(Req = #request{}) ->
  {ok, Bin} = file_read(Req),
  Tree = mochiweb_html:parse(Bin),
  [Bin2 | _] = web_html:findsingle(Tree, Req#request.product_map_target),
  jsx:decode(Bin2).


-spec file_write_product_map(#request{}, map()) -> ok.
file_write_product_map(Req = #request{}, Map) ->
  ok = file:write_file(
    raaw:filename(Req, [{extension, "json"}]),
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.