%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Bike Module behavior
%%%
%%% @end
%%% Created : 27. Jul 2020 7:43 AM
%%%-------------------------------------------------------------------
-module(bike_mod).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("dta/include/records.hrl").

%% TODO: figure out how to organize behavior modules

%% @doc List of URLs
-callback urls() -> [dta_types:url()].

%% @doc Should return the HTML DOM target of the product map
-callback product_map_target() -> {Key, Value} when
  % HTML DOM key, ex: <<"id">>
  Key :: binary(),
  % HTML DOM value, ex: <<"product-id-1">>
  Value :: binary().

-callback variant_inventory_diffs(#request{}) -> [#variant_inventory_diff{}].