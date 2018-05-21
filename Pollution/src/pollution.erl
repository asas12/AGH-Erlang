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

%returns unable_to_add_station or Monitor
addStation(Name, {Long, Lat}, Monitor) ->
  case lists:any(fun(Station) -> (Station#station.name == Name) orelse ((Station#station.coordinates#coordinates.longitude == Long) and (Station#station.coordinates#coordinates.latitude == Lat)) end, Monitor) of
    true -> unable_to_add_station;
    _ ->  Monitor++[#station{name = Name, coordinates = #coordinates{longitude= Long, latitude = Lat}}]
  end.

%returns unable_to_find_station or duplicate_measurement or Monitor
addValue(StationDescriptor, MeasurementDate, MeasurementName, MeasurementValue, Monitor) ->
  {Stat, TmpMonitor} = getStationFromDescriptor(StationDescriptor, Monitor),
  case Stat of
    unable_to_find_station -> unable_to_find_station;
    _ ->  ML = Stat#station.mList,
        case ([Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName]) of
          [] -> NewStat = Stat#station{mList = [#measurement{mName = MeasurementName, mDate = MeasurementDate, mValue = MeasurementValue}|ML]}, TmpMonitor++[NewStat];
          _ -> duplicate_measurement
        end
  end.

%returns unable_to_find_station or Monitor
removeValue(StationDescriptor, MeasurementDate, MeasurementName, Monitor) ->
  {Stat, TmpMonitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  case Stat of
    unable_to_find_station -> unable_to_find_station;
    _ -> ML = Stat#station.mList,
        NewStat = Stat#station{mList = [Measurement || Measurement <- ML, (Measurement#measurement.mDate /= MeasurementDate) or (Measurement#measurement.mName /= MeasurementName)]},
        TmpMonitor++[NewStat]
  end.


%returns unable_to_find_station or no_measurement_matching or monitor
getOneValue(MeasurementName, MeasurementDate, StationDescriptor, Monitor) ->
  {Stat, _Monitor} = getStationFromDescriptor(StationDescriptor, Monitor) ,
  case Stat of
    unable_to_find_station -> unable_to_find_station;
    _ ->  ML = Stat#station.mList,
          Res = ([Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName ]),
          case Res of
            [] -> no_measurement_matching;
            _ -> (hd(([Measurement || Measurement <- ML, Measurement#measurement.mDate == MeasurementDate, Measurement#measurement.mName == MeasurementName ])))#measurement.mValue
          end
  end.

%returns unable_to_find_station or Monitor
getStationMean(StationDescriptor, MeasurementName, Monitor) ->
  {Stat, _Monitor} = getStationFromDescriptor(StationDescriptor, Monitor),
  case Stat of
    unable_to_find_station -> unable_to_find_station;
    _ -> {Sum, No} = lists:foldl(fun(Measurement, {S, N})  when
            Measurement#measurement.mName == MeasurementName -> {S+Measurement#measurement.mValue, N+1};
            (_, {S, N}) -> {S, N} end, {0,1},
          Stat#station.mList),
          Sum/(No-1)
  end.

%returns Monitor
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
  case Elem of
    [] -> {unable_to_find_station, Monitor};
    _ -> {hd(Elem), Monitor -- Elem}
  end;
getStationFromDescriptor(StationD , Monitor) ->
  Elem = [Station || Station <- Monitor, Station#station.name == StationD],
  case Elem of
    [] -> {unable_to_find_station, Monitor};
    _ -> {hd(Elem), Monitor -- Elem}
  end.
