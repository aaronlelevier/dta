%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Entrypoint to scrape a single page.
%%%
%%% @end
%%% Created : 20. Jun 2020 3:59 PM
%%%-------------------------------------------------------------------
-module(scrape).
-author("Aaron Lelevier").
-vsn(1.0).
-export([main/1]).
-include_lib("dta/include/macros.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Version: ~p~n", [init:script_id()]),
    io:format("Args: ~p~n", [Args]),

    [Bike, Url | _] = Args,
    ?LOG({bike, Bike}),
    ?LOG({url, Url}),

    main0(Bike, Url),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

%% main loop
main0(Bike, Url) ->
    start(),
    HasSent = has_notification_sent(Bike),
    ?LOG({notification_sent, HasSent}),

    Outcome =
        if
            HasSent == true ->
                {ok, notification_already_sent};
            true ->
                InStock = is_item_in_stock(Url),
                ?LOG({in_stock, InStock}),

                if
                    InStock == true ->
                        send_notification(Bike),
                        update_table_notification_sent(Bike),
                        {ok, notification_sent};
                    true ->
                        {ok, not_in_stock}
                end
        end,
    done(),
    Outcome.

%% start DETS
start() ->
    ssl:start(),
    inets:start(),
    {ok, ?MODULE} = web_db:start(?MODULE, "web-prod.dets"),
    ok.

%% stop DETS
done() ->
    ok = web_db:done(?MODULE),
    ok.

%% Return bool if a notification has been sent
has_notification_sent(Bike) -> web_db:has_notification_sent(?MODULE, Bike).

%% Return bool if item is in stock
is_item_in_stock(Url) -> web:is_available(Url).

%% send a email
send_notification(Bike) -> email:send_email(Bike, "Is now available").

%% update the table that a notification was sent so we don't send twice
update_table_notification_sent(Bike) ->
    web_db:update_table_notification_sent(?MODULE, Bike).
