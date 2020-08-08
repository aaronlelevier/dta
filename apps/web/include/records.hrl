%% @doc request is a single request object to a URL on a specific date
-record(request, {
  url :: web_request:url(),
  dt :: dateutil:date_string(),
  brand :: atom(),
  bike :: string(),
  product_map_target :: {binary(), binary()}
}).

%% @doc variant is a single unique bike by brand, size, etc..
-record(variant, {
  brand :: string(),
  bike :: string(),
  size :: string(),
  color :: string(),
  price :: integer(),
  id :: integer()
}).

%% @doc inventory is the inventory of a variant on a specific date
-record(inventory, {
  variant_id :: integer(),
  dt :: dateutil:date_string(),
  quantity :: integer()
}).