%%%-------------------------------------------------------------------
%% @doc platfrom public API
%% @end
%%%-------------------------------------------------------------------

-module(platfrom_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
  start/2
  , stop/1]).

-define(App, platfrom).
%%====================================================================
%% API
%%====================================================================
start() ->
  ok = platfrom_util:start_app_deps(?App),
  application:start(?App).

start(_StartType, _StartArgs) ->
  case platfrom_util:start_app_deps(?App) of
    ok ->
       platfrom_mnesia:start();
    _Other ->
      error_logger:info_msg("resource_discovery start error")
  end.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
