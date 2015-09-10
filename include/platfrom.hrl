%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 九月 2015 下午9:23
%%%-------------------------------------------------------------------
-author("apple").

-record(platfrom_room,{room_name="undefined",room_pid="undefined",start_time="undefined",create_user="undefined",create_node=node(),room_max_user=100}).
