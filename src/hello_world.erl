-module(hello_world).

-export([
  start/0,
  stop/0
]).

start() ->
  application:start(crypto),
  application:start(sasl),
  application:start(lager),
  application:start(webmachine),
  application:start(os_mon),
  application:start(riak_sysmon),
  application:start(riak_core),
  application:start(sockjs),
  application:start(cowboy),
  application:start(hello_world).

stop() ->
  ok.
