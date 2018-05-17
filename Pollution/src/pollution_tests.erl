
%%%-------------------------------------------------------------------
%%% @author Gustaw Lippa
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2018 16:38
%%%-------------------------------------------------------------------
-module(pollution_tests).

-author("Gustaw Lippa").

-include_lib("eunit/include/eunit.hrl").

pol_test() ->
  Mon = pollution:createMonitor(),
  ?assertEqual([], Mon),
  P1 = pollution:addStation("Aleje", {50.2345, 18.3445}, Mon),
  ?assertEqual([{station,"Aleje",{coordinates,50.2345, 18.3445},[]}], P1),
  ?assertThrow(unable_to_add_Station, pollution:addStation("Aleje", {50.2345, 18.3445}, [{station,"Aleje",{coordinates,50.1, 18.1},[]}])),
  ?assertThrow(unable_to_add_Station, pollution:addStation("Aleje Słowackiego", {50.2345, 18.3445}, [{station,"Aleje",{coordinates,50.2345, 18.3445},[]}])),
  P2 = pollution:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM10", 51, P1),
  P3 = pollution:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM25", 13, P2),
  ?assertThrow(duplicate_measurement, pollution:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM25", 132, P3)),
  P4 = pollution:addStation("Piastowska", {25.0232, 13.222}, P3),
  ?assertThrow(empty, pollution:getOneValue("PM25", {{2009,9,7},{12,32,22}}, "Piastowska", P4)),
  P5 = pollution:addValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25", 62.03, P4),
  P6 = pollution:addValue({25.0232, 13.222},   calendar:local_time(), "PM10", 100, P5),
  P7 = pollution:removeValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25", P6),
  P8 = pollution:addValue("Aleje", {{2018,4,17},{11,15,00}}, "PM25", 27, P7),
  ?assertEqual(13, pollution:getOneValue("PM25", {{2009,9,7},{12,32,22}}, "Aleje", P8)),
  ?assertEqual(13, pollution:getOneValue("PM25", {{2009,9,7},{12,32,22}}, {50.2345, 18.3445}, P8)),
  P9 = pollution:addValue("Aleje", {{2009,9,7},{12,31,22}}, "PM25", 101, P8),
  ?assert(47 == pollution:getStationMean("Aleje", "PM25", P9)),
  P10 = pollution:addValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25", 62.03, P9),
  ?assertEqual(62.03, pollution:getOneValue("PM25", {{2009,9,7},{12,32,22}}, {25.0232, 13.222}, P10)),
%%  ?assertEqual(176.03/3, pollution:getDailyMean("PM25", {2009,9,7}, P10)),
  ?assertEqual(1,pollution:getDailyOverLimit("PM25", {2009, 9, 7}, 70,  P10)),
  ?assertEqual(0,pollution:getDailyOverLimit("PM25", {{2010, 9, 7},{13,23,12}}, 70,  P10)),
  ?assertEqual(2,pollution:getDailyOverLimit("PM25", {{2009, 9, 7},{13,23,12}}, 0,  P10)).