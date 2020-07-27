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


%% @doc List of URLs
-callback urls() -> [web_request:url()].


%% @doc Should return the HTML DOM target of the product map
-callback product_map_target() -> {Key, Value} when
  % HTML DOM key, ex: <<"id">>
  Key :: binary(),
  % HTML DOM value, ex: <<"product-id-1">>
  Value :: binary().

