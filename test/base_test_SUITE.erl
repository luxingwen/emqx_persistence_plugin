-module(base_test_SUITE).
-compile(export_all).
-define(PID, emqx_persistence_plugin).
-define(APP, emqx_persistence_plugin).
-include_lib("emqx/include/emqx.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, normal},
     {group, ssl}].

groups() ->
    Cases = [client_test, mysql_insert],
    [{normal, [sequence], Cases},
     {ssl, [sequence], Cases}].

init_per_group(ssl, Config) ->
    emqx_ct_helpers:start_apps([emqx_persistence_plugin], fun set_special_configs_ssl/1),
    Config;

init_per_group(normal, Config) ->
    emqx_ct_helpers:start_apps([emqx_persistence_plugin], fun set_special_configs/1),
    Config.

end_per_group(_, Config) ->
    emqx_ct_helpers:stop_apps([emqx_persistence_plugin]),
    Config.

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.


client_test(_Config) ->
    {ok, C} = emqtt:start_link([{host, "127.0.0.1"} ,{clientid, <<"id1">>}]), 
                                % {clientid, <<"Id=001">>}, 
                                % {username, <<"username=username">>},
                                % {password, <<"password=password">>}]),
    {ok, _} = emqtt:connect(C),
    timer:sleep(10),
    emqtt:subscribe(C, <<"test-topic0">>, qos0),
    emqtt:subscribe(C, <<"test-topic0">>),
    timer:sleep(1000),
    emqtt:subscribe(C, <<"test-topic1">>, qos1),
    emqtt:subscribe(C, <<"test-topic1">>),
    timer:sleep(1000),
    emqtt:subscribe(C, <<"test-topic2">>, qos2),
    emqtt:subscribe(C, <<"test-topic2">>),
    timer:sleep(1000),

    emqtt:subscribe(C, <<"$P2P/">>),
    emqtt:subscribe(C, <<"$P2P/test-topic2">>),

    timer:sleep(1000),
    emqtt:publish(C, <<"Topic0">>, <<"HaloWoaod0">>, qos0),
    timer:sleep(1000),
    emqtt:publish(C, <<"Topic1">>, <<"HaloWoaod1">>, qos1),
    timer:sleep(1000),
    emqtt:publish(C, <<"Topic2">>, <<"HaloWoaod2">>, qos2),
    timer:sleep(1000),
    emqtt:publish(C, <<"$P2P/">>, <<"P2PMessage">>, qos2),
    timer:sleep(1000),
    emqtt:publish(C, <<"$P2P/id1">>, <<"P2PMessage">>, qos2),
    timer:sleep(1000),
    emqtt:publish(C, <<"$PERSISTENCE/">>, <<"DataHHHH">>, qos2),
    timer:sleep(1000),
    emqtt:disconnect(C),
    timer:sleep(3000).


mysql_insert(_) ->
    V = ecpool:with_client(?APP, fun(Connection) -> 
                                mysql:query(Connection, <<"select version()">>, []) 
                            end),
    io:format("Version:~p~n",[V]).
    


set_special_configs_ssl(_) ->
    
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    Cfg = application:get_env(emqx_persistence_plugin, server, []),
    SslCfg = [{ssl, {server_name_indication, disable},
                    {cacertfile, emqx_ct_helpers:deps_path(emqx_persistence_plugin, "test/emqx_persistence_plugin_data/ca.pem")},
                    {certfile, emqx_ct_helpers:deps_path(emqx_persistence_plugin, "test/emqx_persistence_plugin_data/client-cert.pem")},
                    {keyfile, emqx_ct_helpers:deps_path(emqx_persistence_plugin, "test/emqx_persistence_plugin_data/client-key.pem")}}],
    application:set_env(emqx_persistence_plugin, server, Cfg ++ SslCfg).

set_special_configs(_) ->
    ok.
