-module(emqx_persistence_mysql_app).

-behaviour(application).

-include("emqx_persistence_mysql.hrl").

-emqx_plugin(?APP).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_persistence_mysql_sup:start_link(),
    ?APP:load([]),
    {ok, Sup}.

stop(_State) ->
    ?APP:unload(),
    mysql:stop(?ECPOOL_WORKER),
    ok.
