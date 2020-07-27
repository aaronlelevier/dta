%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Calls web module functions on the Chromag HTML structure
%%%
%%% @end
%%% Created : 02. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag).
-behavior(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-export([fetch_and_save/1, product_map_target/0, create_request/1, create_request/2]).
-include_lib("dta/include/macros.hrl").
-include_lib("web/include/records.hrl").


%% DEBUG - needed for 'variant' and 'DETS' code below which is still in-flight
-compile(export_all).

%%%%%% bike_mod behavior: start %%%%%%

urls() ->
  chromag_urls:urls().

product_map_target() -> {<<"data-product-json">>, <<"data-product-json">>}.

%%%%%% bike_mod behavior: end %%%%%%


%% @doc fetches and saves the product JSON for all bikes
-spec fetch_and_save_all() -> [ok].
fetch_and_save_all() ->
  [web:fetch_and_save(chromag:create_request(X)) || X <- chromag_urls:urls()].

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


%% Construct the inventory

%% @doc returns a list of Product variants
-spec variants(Url :: string()) -> [map()].
variants(Url) ->
  Req = create_request(Url),
  Map = web_file:product_map(Req),
  Product = maps:get(<<"product">>, Map),
  maps:get(<<"variants">>, Product).


%% @doc Returns product map with inventory counts
-spec inventory(Url :: string()) -> map().
inventory(Url) ->
  Req = create_request(Url),
  Map = web_file:product_map(Req),
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
