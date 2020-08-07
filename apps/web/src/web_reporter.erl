%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Module in charge of receiving reports from works and letting
%%% them know they can exit when their work is done
%%% @end
%%%-------------------------------------------------------------------
-module(web_reporter).
-behaviour(gen_server).

-include_lib("dta/include/macros.hrl").

%% API
-export([start_link/0, notify/1, init_workers/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc reporter will be started at application start
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc notifications from works when they are done are sent here
notify({Pid, Url, BikeMod}) ->
  ?LOG({self(), notify, start}),
  gen_server:cast(?SERVER, {notify, {Pid, Url, BikeMod}}).

%% @doc this initializes all workers for a particular module to do work
init_workers(Mod) ->
  [web_sup:add_worker(X) || X <- Mod:urls()].


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({notify, {Pid, Url, BikeMod}}, State) ->
  ?LOG({self(), report_received, Pid, Url, BikeMod}),
  % reporter tells the worker to exit once their work is done
  exit(Pid, shutdown),
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
