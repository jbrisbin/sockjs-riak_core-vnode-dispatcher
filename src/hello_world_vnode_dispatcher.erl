-module(hello_world_vnode_dispatcher).
-behaviour(gen_server2).

-include("hello_world_vnode_dispatcher.hrl").

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, { site, server, config }).

start_link() ->
  gen_server2:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, infinity}]).

init([]) ->  
  {ok, MasterNode} = application:get_env(master_node),
  
  case node() of
    MasterNode -> ok;
    _ -> riak_core:join(MasterNode)
  end,
  
  WebPort = case application:get_env(web_port) of
    {ok, P} -> P;
    _ -> 3000
  end,
  
  DataState = sockjs_handler:init_state(<<"/data">>, fun sockjs_data_dispatch/2, [{cookie_needed, true}]),
  Routes = [
    % All hosts
    {'_', [
      % Data handler
      {[<<"data">>, '...'], sockjs_cowboy_handler, DataState},
      % Static files
      {[<<"static">>, '...'], cowboy_http_static, [
        {directory, {priv_dir, hello_world, []}},
        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
      ]}
    ]}
  ],
  
  lager:info("Starting SockJS server on ~p", [WebPort]),
  cowboy:start_listener(http, 100, cowboy_tcp_transport, [{port, WebPort}], cowboy_http_protocol, [{dispatch, Routes}]),
  
  {ok, #state {}}.

handle_call(Msg, From, State) ->
  io:format("handle_call: ~p ~p ~p~n", [Msg, From, State]),
  {noreply, State}.

handle_cast(Msg, State) ->
  io:format("handle_cast: ~p ~p~n", [Msg, State]),
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("handle_info: ~p ~p~n", [Msg, State]),
  {noreply, State}.

terminate(Reason, State) ->
  cowboy:stop_listener(http),
  io:format("terminate: ~p ~p~n", [Reason, State]),
  ok.

code_change(OldVsn, State, Extra) ->
  io:format("code_change: ~p ~p ~p~n", [OldVsn, State, Extra]),
  {ok, State}.

sockjs_data_dispatch(Conn, {recv, Data}) ->
  Id = mkid(websocket, data),    
  Hash = riak_core_util:chash_key({<<"data">>, Id}),

  Index = case riak_core_apl:get_primary_apl(Hash, 1, hello_world) of
    [{Idx, _Type}] -> Idx;
    _ -> {0, node()}
  end,
  lager:debug("Dispatching to ~p", [Index]),

  riak_core_vnode_master:command(Index, {sockjs, Data, Conn}, hello_world_vnode_master),

  ok;
sockjs_data_dispatch(_Conn, _) -> ok.
  
mkid(Method, Resource) ->
  % Absconded from riak_core_util:mkclientid/1
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {_,_,NowPart} = now(),
  Id = erlang:phash2([Y,Mo,D,H,Mi,S,Method,Resource,NowPart]),
  io_lib:format("~p", [Id]).
