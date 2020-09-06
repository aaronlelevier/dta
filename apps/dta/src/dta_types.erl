%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Where project wide 'types' live
%%%
%%% @end
%%% Created : 06. Sep 2020 9:58 AM
%%%-------------------------------------------------------------------
-module(dta_types).
-author("Aaron Lelevier").
-vsn(1.0).
-export_type([url/0, dt/0]).

%% URL
-type url() :: string().

%% datetime string of format: "2020-07-04" i.e. "YYYY-MM-DD"
-type dt() :: string().
