%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2012-2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd utility functions. 
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(platfrom_util).

-author("Feng Lee <feng@emqtt.io>").

-export([
  cancel_timer/1,
  now_to_secs/0, now_to_secs/1, start_app_deps/1, sys_information/0, run_time_diff/1]).

cancel_timer(undefined) ->
  undefined;
cancel_timer(Ref) ->
  catch erlang:cancel_timer(Ref).

now_to_secs() ->
  now_to_secs(os:timestamp()).

now_to_secs({MegaSecs, Secs, _MicroSecs}) ->
  MegaSecs * 1000000 + Secs.

%%启动依赖项
%% copy from riak_core_util
%% @spec start_app_deps(App :: atom()) -> ok
%% @doc Start depedent applications of App.
start_app_deps(App) ->
  {ok, DepApps} = application:get_key(App, applications),
  _ = [ensure_started(A) || A <- DepApps],
  ok.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      error_logger:info_msg("Module is :~p,Line is :~p Message is  : ~p", [?MODULE, ?LINE, App]),
      ok;
    {error, {already_started, App}} ->
      ok
  end.
sys_information() ->
  system_information:to_file("systeminfo.txt").

run_time_diff(Fun) ->
  statistics(wall_clock),
  Fun(),
  statistics(wall_clock).


