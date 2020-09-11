%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc File I/O functions and ProductMap helpers
%%%
%%% @end
%%% Created : 25. Jul 2020 7:22 AM
%%%-------------------------------------------------------------------
-module(web_file).
-author("Aaron Lelevier").
-vsn(1.0).
-export([filename/1, filename/2, dirname/1, brand_dirname/1,
  file_read/1, file_write/2, file_delete/1, priv_dir/0,
  product_map/1, product_map/2, file_write_product_map/2, brand_date_dirnames/1]).
-include_lib("dta/include/records.hrl").
-include_lib("dta/include/macros.hrl").

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

%% @doc Returns the abs path to the dated dirname based on the 'Req'
-spec dirname(#request{}) -> string().
dirname(Req = #request{}) ->
  filename:join([
    priv_dir(),
    Req#request.brand,
    Req#request.dt
  ]).

% because 'escript' path for 'code:priv_dir(web)' is different
priv_dir() ->
  filename:join(
    os:getenv("HOME"),
    "Documents/erlang/dta/apps/web/priv"
  ).

%% @doc Returns the abs path to the brand dirname based on the 'Req'
-spec brand_dirname(#request{}) -> string().
brand_dirname(Req = #request{}) ->
  filename:join([
    priv_dir(),
    Req#request.brand
  ]).

%% @doc Returns a list of date dirnames for a brand in DESC order
-spec brand_date_dirnames(#request{}) -> [string()].
brand_date_dirnames(Req = #request{}) ->
  {ok, Filenames} = file:list_dir(brand_dirname(Req)),
  lists:sort(fun(A,B) -> A > B end, Filenames).

-spec file_write(#request{}, [byte()]) -> ok | {error, string()}.
file_write(Req = #request{}, Bytes) ->
  ok = fileutil:make_dir(dirname(Req)),
  ok = file:write_file(filename(Req), Bytes),
  ok.

-spec file_read(#request{}) -> {ok, binary()} | {error, string()}.
file_read(Req = #request{}) ->
  file:read_file(filename(Req)).

%% @doc Gracefully deletes a file whether it exists or not
-spec file_delete(string()) -> ok.
file_delete(Filename) ->
  case filelib:is_regular(Filename) of
    true ->
      file:delete(Filename),
      % assumes that there was only 1 file in the Dir then tries to delete Dir
      file:del_dir(filename:dirname(Filename));
    false ->
      ok
  end.

%% product map of JSON functions %%

-spec product_map(#request{}) -> map().
product_map(Req = #request{}) ->
  product_map(Req, []).

-spec product_map(#request{}, list()) -> map().
product_map(Req = #request{}, Opts) ->
  Ext = proplists:get_value(extension, Opts, "html"),
  ?LOG({extension, Ext}),
  Bin = case Ext of
          "html" ->
            {ok, Bytes} = file_read(Req),
            Tree = mochiweb_html:parse(Bytes),
            [Bin0 | _] = web_html:findsingle(Tree, Req#request.product_map_target),
            Bin0;
          "json" ->
            Filename = filename(Req, [{extension, Ext}]),
            {ok, Bin0} = file:read_file(Filename),
            Bin0
        end,
  jsx:decode(Bin).

-spec file_write_product_map(#request{}, map()) -> ok.
file_write_product_map(Req = #request{}, Map) ->
  ok = file:write_file(
    filename(Req, [{extension, "json"}]),
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.
