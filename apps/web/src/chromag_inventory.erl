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
-export([inventory/1]).
-include_lib("web/include/records.hrl").


inventory(Map) ->
  #inventory{
    variant_id = maps:get(id, Map),
    dt = maps:get(dt, Map),
    quantity = maps:get(inventory_quantity, Map)
  }.