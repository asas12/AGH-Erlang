%%%-------------------------------------------------------------------
%%% @author Gustaw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. maj 2018 13:48
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Gustaw").

%% API
-export([getState/0, start/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyOverLimit/3, stop/0]).

start() ->
  register(polServer, spawn(fun() -> init() end)).

stop() ->
  polServer ! {request, self(), stop}.

init() ->
  loop(pollution:createMonitor()).

% main server loop
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
      Pid ! {reply, ok}
  end.

addStation(Name, {Long, Lat}) ->
  polServer ! {request, self(), addStation, Name, {Long, Lat}},
  receiveAnswer().

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