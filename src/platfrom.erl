%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 九月 2015 下午8:35
%%%-------------------------------------------------------------------
-module(platfrom).
-author("apple").

-include("platfrom.hrl").

%% API
-export([add_room/6, updata_room/6, delete_room/1, get_room/1]).

%%--------------------------------------------------------------------
%% 添加一个房间
%%--------------------------------------------------------------------
add_room(RoomName, RoomPid, StartTime, CreateUser, CreateNode, MaxUser) ->
  case get_room(RoomName) of
    [] ->
      mnesia:dirty_write(#platfrom_room{room_name = RoomName, room_pid = RoomPid, start_time = StartTime, create_user = CreateUser, create_node = CreateNode, room_max_user = MaxUser});
    _Other ->
      error_logger:error_msg("can net add the same roomname Room ~p has room ~p", [RoomName, _Other])
  end.
%%--------------------------------------------------------------------
%% 更新房间
%%--------------------------------------------------------------------
updata_room(RoomName, RoomPid, StartTime, CreateUser, CreateNode, MaxUser) ->
  mnesia:dirty_write(#platfrom_room{room_name = RoomName, room_pid = RoomPid, start_time = StartTime, create_user = CreateUser, create_node = CreateNode, room_max_user = MaxUser}).
%%--------------------------------------------------------------------
%% 删除一个房间
%%--------------------------------------------------------------------
delete_room(RoomName) ->
  mnesia:dirty_delete(platfrom_room, RoomName).

%%--------------------------------------------------------------------
%% 获取一个房间
%%--------------------------------------------------------------------
get_room(RoomName) ->
  mnesia:dirty_read(platfrom_room, RoomName).

