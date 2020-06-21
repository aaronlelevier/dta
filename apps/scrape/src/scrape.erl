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

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Version: ~p~n", [init:script_id()]),
    io:format("Args: ~p~n", [Args]),

    % imports work
    Url = "https://www.commencalusa.com/meta-am-29-c102x3872635",
    File = web:html_filename(Url),
    io:format("File: ~p~n", [File]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
