%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2020 10:46 AM
%%%-------------------------------------------------------------------
-author("Aaron Lelevier").

%% @doc request is a single request object to a URL on a specific date
-record(request, {
  url :: dta_types:url(),
  dt :: dateutil:date_string(),
  brand :: atom(),
  bike :: string(),
  product_map_target :: {binary(), binary()}
}).

%% @doc variant is a single unique bike by brand, size, etc..
-record(variant, {
  id :: integer(),
  brand :: string(),
  bike :: string(),
  size :: string(),
  color :: string(),
  price :: integer()
}).

%% @doc inventory is the inventory of a variant on a specific date
-record(inventory, {
  variant_id :: integer(),
  dt :: dateutil:date_string(),
  quantity :: integer()
}).

%% @doc inventory diff of a variant
-record(inventory_diff, {
  variant_id :: integer(),
  quantity :: integer(),
  prev_quantity :: integer()
}).

%% @doc 'variant' and 'inventory_diff' combined record
-record(variant_inventory_diff, {
  id :: integer(),
  brand :: string(),
  bike :: string(),
  size :: string(),
  color :: string(),
  price :: integer(),
  quantity :: integer(),
  prev_quantity :: integer()
}).