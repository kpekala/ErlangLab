-module(pollution).
-record(station, {coords, name, measurements = #{}}).
-record(mKey, {datetime, type}).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getMinimumDistanceStations/2,
  getMaximumDistanceStations/2, getMaximumTypeValue/2]).

createMonitor() -> [].

addStation(Name, Coord, Stations)->
  Exists = fun (#station{name = Name1, coords = Coord1}) -> (Name1 == Name) or  (Coord1 == Coord) end,
  case lists:any(Exists, Stations) of
    false ->[#station{coords = Coord, name=Name} |Stations];
    _ -> {error, station_exists}
  end.
addMeasurement(Measurements, Date, Type,Value) ->
  Exists = maps:is_key(#mKey{datetime = Date, type = Type}, Measurements),
  case Exists of
    true -> {error, data_exists};
    _ -> maps:put(#mKey{datetime = Date, type = Type}, Value, Measurements)
  end.

addValue( [ St = #station{name= Name, measurements = Measurements} | Stations ], Name, Date, Type, Value) ->
  NewMeasurements = addMeasurement(Measurements, Date, Type, Value),
  case NewMeasurements of
    {error, Message} -> {error, Message};
    _           -> [ St#station{measurements = NewMeasurements} | Stations ]
  end;
addValue( [ St = #station{coords= Coords, measurements = Measurements} | Stations ],Coords, Date, Type, Value) ->
  NewMeasurements = addMeasurement(Measurements, Date, Type, Value),
  case NewMeasurements of
    {error, Message} -> {error, Message};
    _           -> [ St#station{measurements = NewMeasurements} | Stations ]
  end;
addValue( [ H | Stations ], Id, Date, Type, Value) ->
  Result = addValue(Stations, Id, Date, Type, Value),
  case Result of
    {error, Message} -> {error, Message};
    _           -> [ H | Result ]
  end.


removeValue([ St = #station{name= Name, measurements = Measurements} | Stations ], Name, Date, Type) ->
  [St#station{measurements = maps:remove(#mKey{datetime = Date, type = Type}, Measurements)} | Stations ];

removeValue([ St = #station{coords = Coord, measurements = Measurements} | Stations ], Coord, Date, Type) ->
  [St#station{measurements = maps:remove(#mKey{datetime = Date, type = Type}, Measurements)} | Stations ];

removeValue([H|Stations], Id, Date, Type) -> [H|removeValue(Stations, Id, Date, Type)].

getOneValue([#station{name= Name, measurements = Measurements} | _ ], Name, Date, Type)  ->
  maps:get(#mKey{datetime = Date, type = Type}, Measurements, {error, no_such_value});
getOneValue([#station{coords = Coord, measurements = Measurements} | _ ], Coord, Date, Type)  ->
  maps:get(#mKey{datetime = Date, type = Type}, Measurements, {error, no_such_value});
getOneValue([_|Stations], Id, Date, Type) -> getOneValue(Stations, Id, Date, Type).

mean(Sum, N) -> Sum/N.

getMean(Measurements, Type) ->
  Filtered = maps:filter(fun(#mKey{type = T}, _) -> T==Type end, Measurements),
  Sum = maps:fold( fun(_, V, Acc) -> V+Acc end, 0, Filtered),
  mean(Sum, maps:size(Filtered)).

getDailyMean(Stations, Day, Type) ->
  Filter = fun(#station{measurements = Measurements}, Day, Type) ->
    maps:values( maps:filter(fun(#mKey{type = T, datetime = {FDay, _}}, _) -> T==Type andalso FDay==Day end, Measurements) )
           end,
  Values = lists:foldl(fun(St, Acc)-> Filter(St, Day, Type)++Acc end ,[], Stations),
  mean(lists:sum(Values), length(Values)).


getStationMean([#station{name= Name, measurements = Ms} | _ ], Name, Type) -> getMean(Ms, Type);
getStationMean([#station{coords= Coord, measurements = Ms} | _ ], Coord, Type) -> getMean(Ms, Type);
getStationMean([_ | Stations ], Id, Type) -> getStationMean(Stations, Id, Type).

distance({CX, CY}, {X, Y}) -> math:sqrt(math:pow(X - CX,2) + math:pow(Y - CY,2)).

% własne rozszerzenia

%Dostarcza Stację, która leży najbliżej danego punktu na mapie
getMinimumDistanceStations(Stations, {CX, CY}) ->
  DsStations = lists:map(fun(#station{name=Name, coords = Coords})-> {Name, distance({CX, CY}, Coords)} end , Stations),
  MinDs = lists:foldl(fun({_, Val}, Acc)-> min(Val, Acc) end ,1000,DsStations),
  MinDsStation =  lists:filter(fun({_, Val})->Val==MinDs end,DsStations),
  io:format("Minumum distance stations: ~p~n", [lists:map(fun({Name, _})-> Name end, MinDsStation)]),
  MinDsStation.

%Dostarcza Stację, która leży najdalej od danego punktu na mapie
getMaximumDistanceStations(Stations, {CX, CY}) ->
  DsStations = lists:map(fun(#station{name=Name, coords = Coords})-> {Name, distance({CX, CY}, Coords)} end , Stations),
  MaxDs = lists:foldl(fun({_, Val}, Acc)-> max(Val, Acc) end ,0,DsStations),
  MaxDsStation =  lists:filter(fun({_, Val})->Val==MaxDs end,DsStations),
  io:format("Maximum distance stations: ~p~n", [lists:map(fun({Name, _})-> Name end, MaxDsStation)]),
  MaxDsStation.

%Dla danej stacji zwraca maksymalną wartość danego typu pomiaru
getMaximumTypeValue(#station{measurements = Measurements}, Type) ->
  Filtered = maps:filter(fun(#mKey{type = T}, _) -> T==Type end, Measurements),
  Values = maps:values(Filtered),
  MaxValue = lists:foldl(fun(Val, Acc)-> max(Val, Acc) end ,0,Values),
  io:format("Maximum value: ~p~n", [MaxValue]),
  MaxValue.