-module(a).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  a_sup:start_link().

stop(_State) ->
  ok.
