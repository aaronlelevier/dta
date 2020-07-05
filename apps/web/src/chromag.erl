%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Calls web module functions on the Chromag HTML structure
%%%
%%% @end
%%% Created : 02. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).

%% DEBUG
-compile(export_all).


%% NEXT: destructure the map
%% TODO: hardcoded
%% @doc Returns the Product JSON as an Erlang map
%% NOTE: HTML file must already be cached
-spec product_map() -> map().
product_map() ->
  % reads the latest version of thi Url
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  Tree = html_tree(Url),
  Bin = raw_json_data(Tree),
  jsx:decode(Bin).


%% @doc load HTML tree from a local file based on the Url
-spec html_tree(Url :: string()) -> tuple().
html_tree(Url) ->
  {ok, Bin} = read_file(Url),
  mochiweb_html:parse(Bin).


%% @doc destructure the HTML Tree to the JSON binary contents that we care about
-spec raw_json_data(Tree :: tuple()) -> binary().
raw_json_data(Tree) ->
  L = web:findall([<<"html">>, <<"body">>, <<"div">>, <<"main">>, <<"div">>, <<"section">>,
    <<"div">>, <<"div">>, <<"div">>, <<"div">>, <<"script">>], Tree),
  [H | _] = L,
  {_A2, B2} = H,
  [Bin | _] = B2,
  Bin.

%% Construct the inventory

%% @doc returns a list of Product variants
-spec variants() -> [map()].
variants() ->
  Map = product_map(),
  Product = maps:get(<<"product">>, Map),
  maps:get(<<"variants">>, Product).


%% @doc Returns product map with inventory counts
-spec inventory() -> map().
inventory() ->
  Map = product_map(),
  Variants = variants(),
  Inventories = maps:get(<<"inventories">>, Map),
  inventory(Variants, Inventories, []).


inventory([], _Inventories, Acc) -> Acc;
inventory([H | T], Inventories, Acc) ->
  Id = maps:get(<<"id">>, H),
  InvenDetailMap = maps:get(integer_to_binary(Id), Inventories),
  InvenQuantity = maps:get(<<"inventory_quantity">>, InvenDetailMap),
  inventory(T, Inventories, [H#{<<"inventory_quantity">> => InvenQuantity} | Acc]).


%% Populate DETS table

%% @doc a single inventory item to populate the DETS table with
-spec inventory_entry(InvenItem :: map(), Dt :: string()) -> {Key :: binary(), Value :: map()}.
inventory_entry(InvenItem, Dt) ->
  Id = maps:get(<<"id">>, InvenItem),
  Title = maps:get(<<"title">>, InvenItem),
  Quantity = maps:get(<<"inventory_quantity">>, InvenItem),
  {Id, Dt, Title, Quantity}.


db_start() ->
  dets:open_file(?MODULE, [{file, "chromag-prod.dets"}, {type, bag}]).


db_stop() ->
  dets:close(?MODULE).


%% @doc Inserts all items for a given Datetime(Dt) into the DETS table
%% takes the `chromag:inventory()`
db_insert_all([], _Dt) -> ok;
db_insert_all([InvenItem | T], Dt) ->
  ok = dets:insert(?MODULE, inventory_entry(InvenItem, Dt)),
  db_insert_all(T, Dt).


%% Read / Write contents to file %%

%% @doc if you want to fetch the HTML contents and write them to a file
-spec fetch_page_and_write_to_file(Url :: string()) -> ok.
fetch_page_and_write_to_file(Url) ->
  {ok, Body} = web:fetch_page(Url),
  ok = write_html_to_file(Url, Body),
  ok.


-spec write_html_to_file(string(), binary()) -> ok.
write_html_to_file(Url, Body) ->
  % should create the directory for the Page if it doesn't already exist
  ok = maybe_create_bike_html_dir(Url),
  File = html_filename(Url),
  file:write_file(File, Body).


%% @doc reads the latest file written per the Url
-spec read_file(Url :: string()) -> {ok, binary()}.
read_file(Url) ->
  read_file(Url, latest).


%% @doc read latest file per Url or based on a timestamp
-spec read_file(Url, Search) -> {ok, binary()} when
  Url :: string(),
  Search :: {latest | string()}.
read_file(Url, latest) ->
  {ok, L} = file:list_dir(bike_html_dir(Url)),
  L2 = lists:sort(fun(X, Y) -> X > Y end, L),
  [Dt | _] = L2,
  file:read_file(filename:join(bike_html_dir(Url), Dt));
read_file(Url, Dt) ->
  File = html_filename(Url, Dt),
  file:read_file(File).


%% @doc returns the abspath of the filename based on the Url
-spec html_filename(Url :: string()) -> string().
html_filename(Url) ->
  Dt = dateutil:now_timestamp_str(),
  html_filename(Url, Dt).


html_filename(Url, Dt) ->
  Dir = bike_html_dir(Url),
  filename:join([Dir, web:html_page_name(Dt)]).


-spec maybe_create_bike_html_dir(Url :: string()) -> ok.
maybe_create_bike_html_dir(Url) ->
  Dir = bike_html_dir(Url),
  ok = fileutil:maybe_make_dir(Dir),
  ok.


-spec bike_html_dir(Url :: string()) -> string().
bike_html_dir(Url) ->
  Page = web:page_name(Url),
  PrivDir = code:priv_dir(web),
  filename:join([PrivDir, "html", Page]).
