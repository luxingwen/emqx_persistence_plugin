-module(emqx_persistence_mysql_cli).
-behaviour(ecpool_worker).
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include("emqx_persistence_mysql.hrl").
-export([insert/2, connect/1]).
%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------

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

insert(connected, Params) ->
    query(?INSERT_CONNECT_SQL, Params);
insert(disconnected, Params) ->
    query(?INSERT_DISCONNECT_SQL, Params);
insert(publish, Params) ->
    query(?INSERT_PUBLISH_SQL, Params).

query(Sql, Params) ->
    ecpool:with_client(?APP, fun(Connection) ->
                        mysql:query(Connection, Sql, Params)
                    end).
