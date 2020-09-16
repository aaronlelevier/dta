%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Erlang Ports example
%%%
%%% @end
%%% Created : 07. Aug 2020 8:57 AM
%%%-------------------------------------------------------------------
-module(portutil).
-author("Aaron Lelevier").
-vsn(1.0).
-compile(export_all).
-include_lib("dta/include/macros.hrl").

start() ->
  spawn(fun() ->
    register(porttest, self()),
    process_flag(trap_exit, true),
    Filename = "./python_scripts/detect.py",
    Port = open_port({spawn, Filename}, [{packet, 2}, binary]),
    loop(Port)
        end).

stop() ->
  porttest ! stop.

send(X) -> call_port(X).

call_port(Msg) ->
  ?LOG({call_port, Msg}),
  porttest ! {call, self(), encode(Msg)},
  receive
    {porttest, Result} ->
      io:format("received: '~p' ~p ~n", [Result, self()]);
    Other ->
      ?LOG(Other)
  after 1000 ->
    ?LOG(timeout),
    timeout
  end.

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      ?LOG({call, Caller, Msg}),
      Port ! {self(), {command, encode(Msg)}},
      receive
        {Port, Data} ->
          Caller ! {porttest, decode(Data)};
        stop ->
          Port ! {self(), close},
          receive
            {Port, closed} ->
              exit(normal)
          end;
        {'EXIT', Port, Reason} ->
          ?LOG(Reason),
          exit(port_terminated);
        Other ->
          ?LOG(Other)
      end,
      loop(Port)
  end.

encode(X) -> term_to_binary(X).

decode(Data) -> binary_to_term(Data).
