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

%% Construct the inventory

%% @doc returns a list of Product variants
-spec variants(Url :: string()) -> [map()].
variants(Url) ->
  Req = chromag2:create_request(Url),
  Map = web_file:product_map(Req),
  Product = maps:get(<<"product">>, Map),
  maps:get(<<"variants">>, Product).


%% @doc Returns product map with inventory counts
-spec inventory(Url :: string()) -> map().
inventory(Url) ->
  Req = chromag2:create_request(Url),
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
