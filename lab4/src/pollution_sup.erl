-module(pollution_sup).
-author("kgpek").

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link(InitValue) ->
  supervisor:start_link({local, ?SERVER},?MODULE, InitValue).


init(Args) ->
  {ok, {
    {one_for_all, 2, 2000},
    [ {pollution_gen_server,
      {pollution_gen_server, start_link, []},
      permament, brutall_kill, worker, [pollution_gen_server]}
    ]}
  }.