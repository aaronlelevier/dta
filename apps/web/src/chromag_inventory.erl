%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 8:04 AM
%%%-------------------------------------------------------------------
-module(chromag_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-export([inventory/2, build_inventory/1]).
-include_lib("web/include/records.hrl").


-spec inventory(#request{}, map()) -> #inventory{}.
inventory(Req = #request{}, Map) ->
  F = build_inventory(Req),
  F(Map).

-spec build_inventory(#request{}) -> fun((map()) -> #inventory{}).
build_inventory(Req = #request{}) ->
  fun(Map) ->
    #inventory{
      variant_id = maps:get(<<"id">>, Map),
      dt = Req#request.dt,
      quantity = maps:get(<<"inventory_quantity">>, Map)
    }
  end.
