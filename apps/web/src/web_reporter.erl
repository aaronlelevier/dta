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
-export([start_link/0, send_work/1, work_done/1, work_failed/1]).

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

%% @doc start all web_worker's for the BikeMod and wait until complete
-spec send_work(BikeMod) -> ok when
  BikeMod :: chromag | raaw.
send_work(BikeMod) ->
  % tell the workers to do work
  gen_server:call(?SERVER, {send_work, BikeMod}),
  % wait for the workers to finish
  poll(worker_pids()).

%% @doc notifications from works when they are done are sent here
work_done({Pid, Url}) ->
  ?LOG({self(), work_done, start}),
  gen_server:cast(?SERVER, {work_done, {Pid, Url}}).

work_failed({Pid, Url}) ->
  ?LOG({self(), work_failed, Url}),
  gen_server:cast(?SERVER, {work_failed, {Pid, Url}}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #{pids => []}}.

handle_call({send_work, BikeMod}, _From, State) ->
  ?LOG({self(), send_work, BikeMod}),
  % reporter tells the worker to exit once their work is done
  Pids = start_workers(BikeMod),
  {reply, ok, State#{pids => Pids}};
handle_call(wait_for_complete, _From, State) ->
  ?LOG({self(), wait_for_complete}),
  Pids = maps:get(pids, State),
  {reply, Pids, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({Action, {Pid, Url}}, State) when Action == work_done orelse Action == work_failed ->
  ?LOG({self(), Action, Pid, Url, {state, State}}),
  % reporter tells the worker to exit once their work is done
  exit(Pid, shutdown),
  % reporter state updated to remove worker Pid now that they are finished
  {noreply, remove_pid(Pid, State)}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_worker(web_request:url()) -> pid().
start_worker(Url) ->
  {ok, Pid} = web_sup:start_worker(Url),
  Pid.

-spec start_workers(Module) -> [pid()] when
  Module :: raaw | chromag.
start_workers(BikeMod) ->
  [start_worker(X) || X <- BikeMod:urls()].

poll([]) -> ok;
poll(_Pids) ->
  timer:sleep(500),
  poll(worker_pids()).

worker_pids() -> gen_server:call(?SERVER, wait_for_complete).

%% @doc Remove a Pid from the state
remove_pid(Pid, State) ->
  Pids = maps:get(pids, State),
  NewState = State#{pids => lists:delete(Pid, Pids)},
  ?LOG({new_state, NewState}),
  NewState.