%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2020 8:22 AM
%%%-------------------------------------------------------------------
-author("Aaron Lelevier").

-define(LOG, fun(X) -> io:format("Mod:~p Line:~p ~p~n", [?MODULE, ?LINE, X]) end).
