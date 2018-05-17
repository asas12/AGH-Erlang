%%%-------------------------------------------------------------------
%%% @author Gustaw Lippa
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2018 21:14
%%%-------------------------------------------------------------------
-module(pollution).
-author("Gustaw Lippa").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDailyOverLimit/4]).

-record(coordinates, {longitude :: number(), latitude :: number()}).
-record(measurement, {mName :: string(), mDate :: calendar:datetime(), mValue :: number()}).
-record(station, {name:: string(), coordinates, mList = [] :: list(measurement)}).

createMonitor() ->
  [].

addStation(Name, {Long, Lat}, Monitor) ->
  case lists:any(fun(Station) -> (Station#station.name == Name) orelse ((Station#station.coordinates#coordinates.longitude == Long) and (Station#station.coordinates#coordinates.latitude == Lat)) end, Monitor) of
    %true -> erlang:error(badarg);  error czy throw?
    true -> throw(unable_to_add_Station);
    _ -> Monitor++[#station{name = Name, coordinates = #coordinates{longitude= Long, latitude = Lat}}]
  end.


addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue, Monitor) ->
  {Stat, TmpMonitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  ML = Stat#station.mList,
  case ([Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName]) of
    [] -> NewStat = Stat#station{mList = [#measurement{mName = MeasurementName, mDate = MeasurementDate, mValue = MeasurementValue}|ML]},
          TmpMonitor++[NewStat];
    _ -> throw(duplicate_measurement)
  end.

removeValue(StationDescriptor, MeasurementDate, MeasurementName, Monitor) ->
  {Stat, TmpMonitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  ML = Stat#station.mList,
  NewStat = Stat#station{mList = [Measurement || Measurement <- ML, (Measurement#measurement.mDate /= MeasurementDate) or (Measurement#measurement.mName /= MeasurementName)]},
  TmpMonitor++[NewStat].


getOneValue(MeasurementName, MeasurementDate, StationDescriptor, Monitor) ->
  %%{Stat, TmpMonitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  %%ML = Stat#station.mList,
  %%NewStat = Stat#station{mList = [Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName]},
  %%hd(NewStat#station.mList)#measurement.mValue.
  {Stat, _Monitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  ML = Stat#station.mList,
  %%(hd(checkEmpty(lists:filter(fun(Measurement) ->  (Measurement#measurement.mDate == MeasurementDate) and (Measurement#measurement.mName == MeasurementName) end , ML))))#measurement.mValue.
  (hd(checkEmpty([Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName ])))#measurement.mValue.


getStationMean(StationDescriptor, MeasurementName, Monitor) ->
  {Stat, _Monitor} = getStationFromDescriptor(StationDescriptor, Monitor),
  {Sum, No} = lists:foldl(fun(Measurement, {S, N})  when
    Measurement#measurement.mName == MeasurementName -> {S+Measurement#measurement.mValue, N+1};
    (_, {S, N}) -> {S, N} end, {0,1}, Stat#station.mList),

  Sum/(No-1).

getDailyMean(MeasurementName, {Day, _Time}, Monitor) ->
  {Sum, No} = lists:foldl(
    fun(Station, {S, N}) ->
    lists:foldl(
      fun(Measurement, {S2, N2}) when element(1, Measurement#measurement.mDate) == Day, Measurement#measurement.mName == MeasurementName -> {S2+Measurement#measurement.mValue, N2+1};
      (_, {S2, N2}) -> {S2,N2} end,
      {S,N},
      Station#station.mList) end,
    {0,1},
    Monitor),
  Sum/(No-1);

getDailyMean(MeasurementName, {_Year, _Month, Day}, Monitor) ->
  {Sum, No} = lists:foldl(
    fun(Station, {S, N}) ->
      lists:foldl(
        fun(Measurement, {S2, N2}) when element(1, Measurement#measurement.mDate) == Day, Measurement#measurement.mName == MeasurementName -> {S2+Measurement#measurement.mValue, N2+1};
          (_, {S2, N2}) -> {S2,N2} end,
        {S,N},
        Station#station.mList) end,
    {0,1},
    Monitor),
  Sum/(No-1);

getDailyMean(MeasurementName, Day, Monitor) ->
  {Sum, No} = lists:foldl(
    fun(Station, {S, N}) ->
      lists:foldl(
        fun(Measurement, {S2, N2}) when element(1, Measurement#measurement.mDate) == Day, Measurement#measurement.mName == MeasurementName -> {S2+Measurement#measurement.mValue, N2+1};
          (_, {S2, N2}) -> {S2,N2} end,
        {S,N},
        Station#station.mList) end,
    {0,1},
    Monitor),
  Sum/(No-1).

%%Nwm czy podawać normę tak czy będą "skądś" brane
getDailyOverLimit(MeasurementName, {Day, _Time}, Norm, Monitor) ->
  lists:foldl(fun(Station, N) -> summer(Station, N, MeasurementName, Day, Norm) end, 0, Monitor);
getDailyOverLimit(MeasurementName, Date, Norm, Monitor) ->
  %%Summer = fun(Station, N) -> case lists:any( fun(Measurement) when  Measurement#measurement.mName == MeasurementName -> Measurement#measurement.mValue > Norm; (_) -> false end, Station#station.mList) of true -> N+1; false -> N end, end.
  lists:foldl(fun(Station, N) -> summer(Station, N, MeasurementName, Date, Norm) end, 0, Monitor).

summer(Station, N, MeasurementName, Day, Norm) ->
  Elem = [Measurement || Measurement <- Station#station.mList, Measurement#measurement.mName == MeasurementName, element(1, Measurement#measurement.mDate) == Day, Measurement#measurement.mValue > Norm],
  case Elem of
    [] -> N;
    _ -> N+1
  end.

%%pomocnicze funkcje
getStationFromDescriptor({Long, Lat}, Monitor) ->
  Elem = [Station || Station <- Monitor, Station#station.coordinates#coordinates.longitude == Long, Station#station.coordinates#coordinates.latitude == Lat],
  try checkEmpty(Elem) of
    _ -> {hd(checkEmpty(Elem)), Monitor -- Elem}
  catch
    _ -> throw(unable_to_find_station)
  end;
getStationFromDescriptor(StationD , Monitor) ->
  Elem = [Station || Station <- Monitor, Station#station.name == StationD],
  try checkEmpty(Elem) of
    _ -> {hd(checkEmpty(Elem)), Monitor -- Elem}
  catch
    _ -> throw(unable_to_find_station)
  end.

checkEmpty([]) -> throw(empty);
checkEmpty(Arg) -> Arg.