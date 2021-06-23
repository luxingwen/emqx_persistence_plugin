%%%-------------------------------------------------------------------
%% @doc emqx_persistence_plugin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(emqx_persistence_plugin_sup).
-include("emqx_persistence_plugin.hrl").
-define(ECPOOL_WORKER, emqx_persistence_plugin_cli).
-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    case application:get_env(?APP, enable_persistence) of
        {ok, true} ->
            {ok, ServerCfg} = application:get_env(?APP, server),
            PoolSpec = ecpool:pool_spec(?APP, ?APP, ?ECPOOL_WORKER, ServerCfg),
            {ok, {{one_for_one, 10, 100}, [PoolSpec]}};
        _ ->
            {ok, {{one_for_one, 10, 100}, []}}
    end.
