%%%-------------------------------------------------------------------
%%% @author glippa
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 11:36 AM
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("glippa").

%% API
-export([start/0]).



start() ->
  spawn(fun() -> init() end).

init() ->
  process_flag(trap_exit, true),
  pollution_server:start(),
  %pollution_gen_server:start(),
  receive
    {'EXIT', Pid, Reason} -> io:format("Error: ~w~n", [Reason]),
      init()
  end.

