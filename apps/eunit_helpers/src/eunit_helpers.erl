%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Eunit test helpers
%%%
%%% @end
%%% Created : 27. Jul 2020 7:43 AM
%%%-------------------------------------------------------------------
-module(eunit_helpers).
-author("Aaron Lelevier").
-vsn(1.0).
-export([url/0, raaw_url/0, req/0, raaw_req/0, product_map/0, bike_map/0]).
-include_lib("dta/include/records.hrl").

-spec url() -> string().
url() ->
  "https://chromagbikes.com/collections/27-5-26/products/stylus-2020".

-spec raaw_url() -> string().
raaw_url() ->
  raaw_urls:madonna_v2_frame_kit().

%% TODO: might want to change name to 'chromag_req'
-spec req() -> #request{}.
req() ->
  web:create_request(url(), [{dt, dateutil:date_str(2020, 8, 8)}]).

raaw_req() ->
  Url = raaw_urls:madonna_v2_frame_kit(),
  web:create_request(Url, [{dt, dateutil:date_str(2020, 7, 19)}]).

-spec product_map() -> map().
product_map() ->
  web_file:product_map(req(), [{extension, "json"}]).

-spec bike_map() -> map().
bike_map() ->
  [H | _] = chromag_product_map:bike_maps(req()),
  H.
