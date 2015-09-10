%%%-------------------------------------------------------------------
%% @doc platfrom top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(platfrom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Children = [],
  platfrom_mnesia:start(),
  {ok, {{one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
