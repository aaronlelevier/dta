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
-include_lib("dta/include/macros.hrl").
-include_lib("web/include/records.hrl").

%% DEBUG
-compile(export_all).

%% TESTING
stylus_url() -> "https://chromagbikes.com/collections/27-5-26/products/stylus-2020".

%% #request{} constructors
-spec create_request(web_request:url()) -> #request{}.
create_request(Url) -> #request{url = Url, dt = dateutil:now_timestamp_str()}.

-spec create_request(web_request:url(), web_request:dt()) -> #request{}.
create_request(Url, Dt) -> #request{url = Url, dt = Dt}.


%% web_request impl interface: start
filename(Req) ->
  html_filename(Req#request.url, Req#request.dt).

%% web_request impl interface: stop


main(Url) ->
  {ok, ?MODULE} = db_start(),
  % need to fetch and save to a file because all HTML manipulation uses File I/O
  fetch_page_and_write_to_file(Url),
  Inventory = inventory(Url),
  Dt = dateutil:now_timestamp_str(),
  ok = db_insert_all(Inventory, Dt),
  ok = db_stop().


%% NEXT: destructure the map
%% TODO: hardcoded
%% @doc Returns the Product JSON as an Erlang map
%% NOTE: HTML file must already be cached
-spec product_map(Url :: string()) -> map().
product_map(Url) ->
  % reads the latest version of thi Url
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
-spec variants(Url :: string()) -> [map()].
variants(Url) ->
  Map = product_map(Url),
  Product = maps:get(<<"product">>, Map),
  maps:get(<<"variants">>, Product).


%% @doc Returns product map with inventory counts
-spec inventory(Url :: string()) -> map().
inventory(Url) ->
  Map = product_map(Url),
  Variants = variants(Url),
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


%% TODO: Table doesn't contain the Bike name!!
db_start() ->
  dets:open_file(?MODULE, [{file, "chromag-prod.dets"}, {type, bag}]).


db_stop() ->
  dets:close(?MODULE).


%% @doc Inserts all items for a given Datetime(Dt) into the DETS table
%% takes the `chromag:inventory(Url)`
db_insert_all([], _Dt) -> ok;
db_insert_all([InvenItem | T], Dt) ->
  ok = dets:insert(?MODULE, inventory_entry(InvenItem, Dt)),
  db_insert_all(T, Dt).


db_log_changed_inventories() ->
  Id = dets:first(?MODULE),
  ok = db_log_changed_inventories(Id).

%% TODO: instead should check if something came back in stock
db_log_changed_inventories('$end_of_table') -> ok;
db_log_changed_inventories(Id) ->
  Entries = dets:lookup(chromag, Id),
  if length(Entries) =:= 2 ->
    [H, H2 | _] = Entries,
    if element(4, H) =/= element(4, H2) ->
      ?LOG({inventory_change, Id});
      true ->
        ?LOG({no_change, Id})
    end;
    true -> ?LOG({entry_count, length(Entries), Entries})
  end,
  Next = dets:next(?MODULE, Id),
  db_log_changed_inventories(Next).


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
