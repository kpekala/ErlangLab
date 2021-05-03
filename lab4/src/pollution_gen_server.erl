-module(pollution_gen_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, stop/0, crash/0]).

-define(SERVER, ?MODULE).

-record(pollution_gen_server_state, {}).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?SERVER, terminate).
crash() -> gen_server:cast(?SERVER, crash).

init([]) ->
  {ok, #pollution_gen_server_state{}}.

handle_call(_Request, _From, Monitor) ->
  {reply, ok, Monitor}.

handle_cast(_Request, State = #pollution_gen_server_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #pollution_gen_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
  ok.

code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
  {ok, State}.

getMonitor() -> gen_server:call(?SERVER, getMonitor).
addStation(Name, Coord) -> gen_server:call(?SERVER, {addStation, Name, Coord}).
addValue(Id, Date, Type, Value) -> gen_server:call(?SERVER,{addValue, Id, Date, Type, Value}).
getOneValue(Id, Date, Type) -> gen_server:call(?SERVER,{getOneValue,Id, Date, Type}).

secure(UpdatedMonitor, Monitor)->
    case UpdatedMonitor of
        {error, _} ->
            {reply, UpdatedMonitor, Monitor};
        _ ->
            {reply, ok, UpdatedMonitor}
    end.

handle({getMonitor}, Pid, Monitor) -> {reply, Monitor, Monitor};

handle({addStation, Name, Coord}, Pid, Monitor) ->
    UpdatedMonitor = pollution:addStation(Monitor, Name, Coord),
    secure(UpdatedMonitor, Monitor);

handle({addValue, Id, Date, Type, Value}, Pid, Monitor) ->
    UpdatedMonitor = pollution:addValue(Monitor,Id, Date, Type, Value),
    secure(UpdatedMonitor, Monitor);

handle({getOneValue,Id, Date, Type},Pid, Monitor) -> {reply,pollution:getOneValue(Monitor,Id,Date,Type)}.
