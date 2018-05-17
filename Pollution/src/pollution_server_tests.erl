%%%-------------------------------------------------------------------
%%% @author Gustaw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. maj 2018 19:57
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("Gustaw").

-include_lib("eunit/include/eunit.hrl").

pol_test() ->
  pollution_server:start(),
  pollution_server:addStation("Aleje", {50.2345, 18.3445}),
  ?assertEqual([{station,"Aleje",{coordinates,50.2345, 18.3445},[]}], pollution_server:getState()),
  %?assertThrow(unable_to_add_Station, pollution_server:addStation("Aleje", {50.2345, 18.3445})),
  %?assertThrow(unable_to_add_Station, pollution_server:addStation("Aleje Słowackiego", {50.2345, 18.3445})),
  pollution_server:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM10", 51),
  pollution_server:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM25", 13),
  %?assertThrow(duplicate_measurement, pollution_server:addValue("Aleje", {{2009,9,7},{12,32,22}}, "PM25", 132)),
  pollution_server:addStation("Piastowska", {25.0232, 13.222}),
  %?assertThrow(empty, pollution_server:getOneValue("PM25", {{2009,9,7},{12,32,22}}, "Piastowska")),
  pollution_server:addValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25", 62.03),
  pollution_server:addValue({25.0232, 13.222},   calendar:local_time(), "PM10", 100),
  pollution_server:removeValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25"),
  pollution_server:addValue("Aleje", {{2018,4,17},{11,15,00}}, "PM25", 27),
  ?assertEqual(13, pollution_server:getOneValue("PM25", {{2009,9,7},{12,32,22}}, "Aleje")),
  ?assertEqual(13, pollution_server:getOneValue("PM25", {{2009,9,7},{12,32,22}}, {50.2345, 18.3445})),
  pollution_server:addValue("Aleje", {{2009,9,7},{12,31,22}}, "PM25", 101),
  %?assert(47 == pollution_server:getStationMean("Aleje", "PM25", P9)),
  pollution_server:addValue("Piastowska", {{2009,9,7},{12,32,22}}, "PM25", 62.03),
  ?assertEqual(62.03, pollution_server:getOneValue("PM25", {{2009,9,7},{12,32,22}}, {25.0232, 13.222})),
%%  ?assertEqual(176.03/3, pollution:getDailyMean("PM25", {2009,9,7}, P10)),
  ?assertEqual(1, pollution_server:getDailyOverLimit("PM25", {2009, 9, 7}, 70)),
  ?assertEqual(0, pollution_server:getDailyOverLimit("PM25", {{2010, 9, 7},{13,23,12}}, 70)),
  ?assertEqual(2, pollution_server:getDailyOverLimit("PM25", {{2009, 9, 7},{13,23,12}}, 0)).