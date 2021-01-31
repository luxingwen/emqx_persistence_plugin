-module(emqx_persistence_plugin_cli).
-behaviour(ecpool_worker).
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include("emqx_persistence_plugin.hrl").
-export([insert/2, connect/1]).
%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------
-define(INSERT_CONNECT_SQL, <<"INSERT INTO `mqtt`.`on_client_connected`(`client_id`, `username`, `host`) VALUES (?, ? ,?);">>).
-define(INSERT_DISCONNECT_SQL, <<"INSERT INTO `mqtt`.`on_client_disconnected`(`client_id`, `username`, `host`, `reason`) VALUES (?, ?, ?, ?);">>).
-define(INSERT_PUBLISH_SQL, <<"INSERT INTO `mqtt`.`on_client_publish`( `client_id`, `username`,`host` , `msg_id`, `dup`, `retain`, `qos`, `tpoic`, `payload`, `ts`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);">>).

connect(Options) ->
    case mysql:start_link(Options) of
        {ok, Pid} -> 
            {ok, Pid};
        ignore -> 
            {error, ignore};
        {error, Reason = {{_, {error, econnrefused}}, _}} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: Connection refused."),
            {error, Reason};
        {error, Reason = {ErrorCode, _, Error}} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: ~p - ~p", [ErrorCode, Error]),
            {error, Reason};
        {error, Reason} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: ~p", [Reason]),
            {error, Reason};
        Other -> Other
    end.

insert(connect, Params) ->
    query(?INSERT_CONNECT_SQL, Params);
insert(disconnect, Params) ->
    query(?INSERT_DISCONNECT_SQL, Params);
insert(publish, Params) ->
    query(?INSERT_PUBLISH_SQL, Params).

query(Sql, Params) when is_list(Params) ->
    ecpool:with_client(?APP, fun(Connection) -> 
                                mysql:query(Connection, Sql, Params) 
                             end).