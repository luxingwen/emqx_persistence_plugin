%%%-------------------------------------------------------------------
%% @doc emqx_persistence_mysql top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(emqx_persistence_mysql_sup).
-include("emqx_persistence_mysql.hrl").
-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, ServerCfg} = application:get_env(?APP, server),
    PoolSpec = ecpool:pool_spec(?APP, ?APP, ?ECPOOL_WORKER, ServerCfg),
    {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.
