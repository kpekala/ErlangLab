-module(main).
-author("kgpek").

-behaviour(application).

-export([start/2,
  stop/1]).

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  case 'TopSupervisor':start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.


-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.
