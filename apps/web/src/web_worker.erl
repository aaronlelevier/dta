%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Module doing the actual work of fetching web pages and
%%% persisting the results.
%%% There should be multiple workers so work can get done in parallel
%%% @end
%%%-------------------------------------------------------------------
-module(web_worker).
-behaviour(gen_server).

-include_lib("dta/include/macros.hrl").

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Url) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Url], []),
  % use the brand to lookup the BikeMod
  BikeMod = web_url:bike_mod(Url),
  fetch_page(Pid, {Url, BikeMod}),
  {ok, Pid}.


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([Url]=Args) ->
  ?LOG(Args),
  {ok, Url}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send_report, {Url, BikeMod}}, State) ->
  ?LOG({state, State}),
  ?LOG({self(), report_sent, Url, BikeMod}),
  % fetch the web page and store results
  ok = web:fetch_single(BikeMod, Url),
  % notify reporter our work is done
  web_reporter:notify({self(), Url, BikeMod}),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_page(Pid, {Url, BikeMod}) ->
  ?LOG({self(), send_report, start}),
  gen_server:cast(Pid, {send_report, {Url, BikeMod}}).
