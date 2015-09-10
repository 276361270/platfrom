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
%%% emqttd mnesia.
%%% copy from emqttd nad modify
%%% @end
%%%-----------------------------------------------------------------------------

-module(platfrom_mnesia).

-author('feng@emqtt.io').

-include("platfrom.hrl").

%%-include("emqttd.hrl").

-export([start/0]).

-export([create_table/2, copy_table/1]).

-define(WAIT_FOR_TABLES, 10000).

start() ->
  case init_schema() of
    ok ->
      ok;
    {error, {_Node, {already_exists, _Node}}} ->
      ok;
    {error, Reason} ->
      lager:error("mnesia init_schema error: ~p", [Reason])
  end,
  ok = mnesia:start(),
  init_tables(),
  add_extra_nodes(nodes()).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% init mnesia schema.
%%
%% @end
%%------------------------------------------------------------------------------
init_schema() ->
  case mnesia:system_info(extra_db_nodes) of
    [] ->
      %% create schema
      mnesia:create_schema([node()]);
    __ ->
      ok
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% init mnesia tables.
%%
%% @end
%%------------------------------------------------------------------------------
init_tables() ->
  case mnesia:system_info(extra_db_nodes) of
    [] ->
      create_tables(),
      create();
    _ ->
      copy_tables()
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% create tables.
%%
%% @end
%%------------------------------------------------------------------------------
create_tables() ->
  platfrom_util:apply_module_attributes(boot_mnesia).

create_table(Table, Attrs) ->
  case mnesia:create_table(Table, Attrs) of
    {atomic, ok} -> ok;
    {aborted, {already_exists, Table}} -> ok;
    Error -> Error
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% copy tables.
%%
%% @end
%%------------------------------------------------------------------------------
copy_tables() ->
  platfrom_util:apply_module_attributes(copy_mnesia).

copy_table(Table) ->
  case mnesia:add_table_copy(Table, node(), ram_copies) of
    {atomic, ok} -> ok;
    {aborted, {already_exists, Table, _Node}} -> ok;
    {aborted, Error} -> Error
  end.

create() ->
  create_table(platfrom_room, [{attributes, record_info(fields, platfrom_room)}]).

copy() ->
  copy_table(platfrom_room).

add_extra_nodes([Node | T]) ->
  case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, [Node]} ->
      copy(),
      Tables = mnesia:system_info(tables),
      mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
    _ ->
      add_extra_nodes(T)
  end.