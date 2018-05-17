%%%-------------------------------------------------------------------
%%% @author glippa
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 12:13 PM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("glippa").

%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([getState/0, crash/0, start/0, addStation/2, addValue/4, stop/0]).

start() ->
  gen_server:start_link({local, pollution_gen_server}, pollution_gen_server, pollution:createMonitor(), []).
  %register(polServer, spawn_link(fun() -> init() end)).

stop() ->
  gen_server:cast(pollution_gen_server, stop).

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n",[Value]),
  Reason.

init(InitialValue) ->
  {ok, InitialValue}.

crash() ->
  gen_server:cast(pollution_gen_server, crash).

addStation(Name, {Long, Lat}) ->
  gen_server:call(pollution_gen_server, {addStation, Name, {Long, Lat}}).

handle_cast(stop, Value) ->
  {stop, normal, Value};
handle_cast(crash, State) ->
  10/0.

%na razie bez obsługi błędów.... zmienić w pollution na atomy errory
handle_call({addStation, Name, {Long, Lat}}, From, State) ->
  NewState = pollution:addStation(Name, {Long, Lat}, State),
  {reply, NewState, NewState}.

addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue) ->
  polServer ! {request, self(),addValue, StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue},
  receiveAnswer().

removeValue(StationDescriptor, MeasurementDate, MeasurementName) ->
  polServer ! {request, self(), removeValue, StationDescriptor, MeasurementDate, MeasurementName},
  receiveAnswer().

getOneValue(MeasurementName, MeasurementDate, StationDescriptor) ->
  polServer ! {request, self(), getOneValue, MeasurementName, MeasurementDate, StationDescriptor},
  receiveAnswer().

getStationMean(StationDescriptor, MeasurementName) ->
  polServer ! {request, self(),getStationMean,StationDescriptor, MeasurementName},
  receiveAnswer().

getDailyMean(MeasurementName, {Day, _Time}) ->
  polServer ! {request, self(),getDailyMean,MeasurementName, {Day, _Time}},
  receiveAnswer();
getDailyMean(MeasurementName, {_Year, _Month, Day}) ->
  polServer ! {request, self(),getDailyMean,MeasurementName, {_Year, _Month, Day}},
  receiveAnswer();
getDailyMean(MeasurementName, Day) ->
  polServer ! {request, self(),getDailyMean,MeasurementName, Day},
  receiveAnswer().

getDailyOverLimit(MeasurementName, {Day, _Time}, Norm) ->
  polServer ! {request, self(),getDailyOverLimit,MeasurementName, {Day, _Time}, Norm},
  receiveAnswer();
getDailyOverLimit(MeasurementName, Date, Norm) ->
  polServer ! {request, self(),getDailyOverLimit,MeasurementName, Date, Norm},
  receiveAnswer().

getState() ->
  polServer ! {request, self(), getState},
  receive
    {reply, State} -> %io:format("State: ~w~n", [State]),
      State
  end.

receiveAnswer() ->
  receive
    {reply, ok} ->
      %getState(),
      ok;
    {reply, State} -> State;
    _ -> throw(unexpected_answer)
  end.

loop(State) ->
  receive
    {request, Pid, addStation, Name, {Long, Lat}} ->
      Pid ! {reply, ok},
      loop(pollution:addStation(Name, {Long, Lat}, State));
    {request, Pid, addValue, StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue} ->
      Pid ! {reply, ok},
      loop(pollution:addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue, State));
    {request, Pid, removeValue, StationDescriptor, MeasurementDate, MeasurementName} ->
      Pid ! {reply, ok},
      loop(pollution:removeValue(StationDescriptor, MeasurementDate, MeasurementName, State));
    {request, Pid, getOneValue, MeasurementName, MeasurementDate, StationDescriptor} ->
      Pid ! {reply, pollution:getOneValue(MeasurementName, MeasurementDate, StationDescriptor, State)},
      loop(State);
    {request, Pid, getStationMean, StationDescriptor, MeasurementName} ->
      Pid ! {reply, pollution:getStationMean(StationDescriptor, MeasurementName, State)},
      loop(State);
    {request, Pid, getDailyMean, MeasurementName, Date} ->
      Pid ! {reply, pollution:getDailyMean(MeasurementName, Date, State)},
      loop(State);
    {request, Pid, getDailyOverLimit, MeasurementName, Date, Norm} ->
      Pid ! {reply, pollution:getDailyOverLimit(MeasurementName, Date, Norm, State)},
      loop(State);
    {request, Pid, getState} ->
      Pid ! {reply, State},
      loop(State);
    {request, Pid, stop}->
      {reply, ok};
    {request, Pid, crash} ->
      10/0
  end.