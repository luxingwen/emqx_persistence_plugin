-module(base_test_SUITE).
-compile(export_all).
-include_lib("emqx/include/emqx.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> emqx_ct:all(?MODULE).

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_persistence_mysql]),
    Config.

end_per_suite(_) ->
    emqx_ct_helpers:stop_apps([emqx_persistence_mysql]),
    ok.

t_save_to_mysql(_) ->
    {ok, C1} = emqtt:start_link([{host, "127.0.0.1"} ,
                                 {clientid, <<"demo-clientid1">>},
                                 {username, <<"demo-username1">>},
                                 {password, <<"demo-password1">>}]),
    {ok, _} = emqtt:connect(C1),
    {ok, 2} = emqtt:publish(C1, <<"$MYSQL">>, <<"test-is-ok">>, qos1),
    ok = emqtt:disconnect(C1),
    timer:sleep(2000).