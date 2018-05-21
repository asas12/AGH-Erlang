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
-export([getState/0, crash/0, start/0, addStation/2, addValue/4, stop/0, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyOverLimit/3]).

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

addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue) ->
  gen_server:call(pollution_gen_server, {addValue, StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue}).

removeValue(StationDescriptor, MeasurementDate, MeasurementName) ->
  gen_server:call(pollution_gen_server, {removeValue, StationDescriptor, MeasurementDate, MeasurementName}).

getOneValue(MeasurementName, MeasurementDate, StationDescriptor) ->
  gen_server:call(pollution_gen_server, {getOneValue, MeasurementName, MeasurementDate, StationDescriptor}).

getStationMean(StationDescriptor, MeasurementName) ->
  gen_server:call(pollution_gen_server, {getStationMean, StationDescriptor, MeasurementName}).

getDailyMean(MeasurementName, Date) ->
  gen_server:call(pollution_gen_server, {getDailyMean, MeasurementName, Date}).

getDailyOverLimit(MeasurementName, Date, Norm) ->
  gen_server:call(pollution_gen_server, {getDailyOverLimit, MeasurementName, Date, Norm}).

getState() ->
  gen_server:call(pollution_gen_server, getState).

handle_cast(stop, Value) ->
  {stop, normal, Value};
handle_cast(crash, _State) ->
  10/0.

%na razie bez obsługi błędów.... zmienić w pollution na atomy errory
handle_call({addStation, Name, {Long, Lat}}, _From, State) ->
  NewState = pollution:addStation(Name, {Long, Lat}, State),
  case NewState of
    unable_to_add_station -> {reply, unable_to_add_station, State};
    _ -> {reply, NewState, NewState}
  end;

handle_call({addValue, StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue}, _From, State) ->
  NewState = pollution:addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue, State),
  case NewState of
    unable_to_find_Station -> {reply, unable_to_find_Station, State};
    duplicate_measurement -> {reply, duplicate_measurement, State};
    _ -> {reply, NewState, NewState}
  end;

handle_call({removeValue, StationDescriptor, MeasurementDate, MeasurementName}, _From, State)->
  NewState = pollution:removeValue(StationDescriptor,MeasurementDate, MeasurementName, State),
  case NewState of
    unable_to_find_station -> {reply, unable_to_find_station, State};
    _ -> {reply, NewState, NewState}
  end;

handle_call({getOneValue, MeasurementName, MeasurementDate, StationDescriptor}, _From, State)->
  NewState = pollution:getOneValue/4(MeasurementName, MeasurementDate, StationDescriptor, State),
  case NewState of
    unable_to_find_station -> {reply, unable_to_find_station, State};
    no_measurement_matching -> {reply, no_measurement_matching, State};
    _ -> {reply, NewState, State}
  end;

handle_call({getStationMean, StationDescriptor, MeasurementName}, _From, State)->
  NewState = pollution:getStationMean(StationDescriptor,MeasurementName, State),
  {reply, NewState, State};

handle_call({getDailyMean, MeasurementName, Date}, _From, State)->
  NewState = pollution:getDailyMean(MeasurementName,Date, State),
 {reply, NewState, State};

handle_call({getDailyOverLimit, MeasurementName, Date, Norm}, _From, State)->
  NewState = pollution:getDailyOverLimit(MeasurementName, Date, Norm, State),
  {reply, NewState, State}.


