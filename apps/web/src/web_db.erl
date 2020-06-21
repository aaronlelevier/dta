%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Use a DETS table as a cache so stop scraping and only send
%%% one notifications once the item is in stock
%%%
%%% @end
%%% Created : 21. Jun 2020 6:43 AM
%%%-------------------------------------------------------------------
-module(web_db).
-author("Aaron Lelevier").
-vsn(1.0).
-export([start/2, done/1, has_notification_sent/2, update_table_notification_sent/2]).

%% start DETS
-spec start(Name, File) -> {ok, Name} when
  Name :: atom(),
  File :: string().
start(Name, File) -> dets:open_file(Name, [{file, File}]).

%% stop DETS
-spec done(Name::atom()) -> ok.
done(Name) -> dets:close(Name).

%% Return bool if a notification has been sent
-spec has_notification_sent(Name::atom(), Bike::string()) -> boolean().
has_notification_sent(Name, Bike) ->
  case dets:lookup(Name, list_to_atom(Bike)) of
    L when length(L) > 0 -> true;
    _ -> false
  end.

%% update the table that a notification was sent so we don't send twice
-spec update_table_notification_sent(Name::atom(), Bike::string()) -> ok.
update_table_notification_sent(Name, Bike) ->
  dets:insert(Name, {list_to_atom(Bike)}).
