%%%-------------------------------------------------------------------
%% @doc emqx_persistence_plugin public API
%% @end
%%%-------------------------------------------------------------------

-module(emqx_persistence_plugin_app).

-behaviour(application).

-include("emqx_persistence_plugin.hrl").

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_persistence_plugin_sup:start_link(),
    ?APP:load(),
    ?APP:register_metrics(),
    emqx:hook('client.authenticate', fun emqx_persistence_plugin:on_client_authenticate/2, []),
    {ok, Sup}.

stop(_State) ->
    ?APP:unload(),
    emqx:unhook('client.authenticate', fun emqx_persistence_plugin:on_client_authenticate/2),
    ok.
