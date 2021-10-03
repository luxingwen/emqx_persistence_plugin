-module(emqx_persistence_plugin).

-include("emqx_persistence_plugin.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ register_metrics/0, load/0, unload/0]).
-export([ on_client_connected/3, on_client_disconnected/4]).
-export([ on_message_publish/2]).
-export([ on_client_subscribe/4, on_client_unsubscribe/4]).

register_metrics() ->
    [emqx_metrics:new(MetricName)
    || MetricName <- ['emqx_persistence_plugin.client_connected',
                      'emqx_persistence_plugin.client_disconnected',
                      'emqx_persistence_plugin.client_subscribe',
                      'emqx_persistence_plugin.client_unsubscribe',
                      'emqx_persistence_plugin.message_publish']].

load() ->
    lists:foreach(
        fun({Hook, Fun, Filter}) ->
            load_(Hook, binary_to_atom(Fun, utf8), {Filter})
        end, parse_rule(application:get_env(?APP, hooks, []))).

unload() ->
    lists:foreach(
        fun({Hook, Fun, _Filter}) ->
            unload_(Hook, binary_to_atom(Fun, utf8))
        end, parse_rule(application:get_env(?APP, hooks, []))).

%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(#{clientid := ClientId, username := Username, peerhost := {B1, B2, B3, B4}}, _ConnInfo, _Env) ->
    emqx_metrics:inc('emqx_persistence_plugin.client_connected'),
    F = fun (X) -> case X of undefined -> <<"undefined">>; _ -> X  end end,
    IP =  io_lib:format("~B.~B.~B.~B",[B1, B2, B3, B4]),
    emqx_persistence_plugin_cli:insert(connect, [F(ClientId), F(Username), IP]),
    ok.
%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------

on_client_disconnected(#{clientid := ClientId, username := Username},
                       Reason,
                       #{peername := {{B1, B2, B3, B4}, Port}}, _Env) ->

    emqx_metrics:inc('emqx_persistence_plugin.client_disconnected'),
    F1 = fun(R) -> case is_atom(R) of true -> atom_to_binary(R, utf8); _ -> <<"normal">> end end,
    F2 = fun (X) -> case X of undefined -> <<"undefined">>; _ -> X  end end,
    IP =  io_lib:format("~B.~B.~B.~B:~B",[B1, B2, B3, B4, Port]),
    emqx_persistence_plugin_cli:insert(disconnect, [F2(ClientId), F2(Username), IP, F1(Reason)]),
    ok;

on_client_disconnected(_, Reason, _ConnInfo, _Env) ->
    ?LOG(error, "Client disconnected, cannot encode reason: ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------

on_client_subscribe(#{clientid := _ClientId, username := _Username}, _Properties, RawTopicFilters, {_Filter}) ->
    lists:foreach(fun({Topic, _Opts}) ->
        emqx_metrics:inc('emqx_persistence_plugin.client_subscribe'),
        case string:left(erlang:binary_to_list(Topic), 5) of
            "$P2P/" ->
                {stop, deny};
            _ ->
                {matched, allow}
        end
    end, RawTopicFilters).

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------
on_client_unsubscribe(#{clientid := _ClientId, username := _Username}, _Properties, RawTopicFilters, {Filter}) ->
    lists:foreach(fun({Topic, _Opts}) ->
        with_filter(
            fun() ->
                emqx_metrics:inc('emqx_persistence_plugin.client_unsubscribe'),
                ok
        end, Topic, Filter)
    end, RawTopicFilters).

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

%% Append clientId
on_message_publish(#message{id = MsgId,
                            qos = Qos,
                            topic = <<"$WITH_CLIENTID/",_/binary>> = Topic,
                            from = From,
                            flags = Flags,
                            headers = Headers,
                            payload = Payload,
                            timestamp = Ts}, _Env) ->
    NewMessage = #message{id = MsgId,
                          qos = Qos,
                          topic = Topic,
                          from = From,
                          flags = Flags,
                          headers = Headers,
                          payload = <<From/binary, "#", Payload/binary>>,
                          timestamp = Ts},
    {ok, NewMessage};
% persitences to mysql
on_message_publish(Message = #message{id = <<I1:64, I2:48, I3:16>> = _MsgId,
                                      qos = Qos, 
                                      topic = <<"$PERSISTENCE/", _/binary>> = Topic,
                                      from = From,
                                      flags = #{dup := Dup, retain := Retain},
                                      headers = #{peerhost := {B1, B2, B3, B4}, username := Username},
                                      payload = Payload,
                                      timestamp = Ts},
                                      _Env) ->
    emqx_metrics:inc('emqx_persistence_plugin.message_publish'),
    F1 = fun(X) -> case X of  true ->1; _ -> 0 end end,
    F2 = fun (X) -> case X of undefined -> <<"undefined">>; _ -> X  end end,

    ok = emqx_persistence_plugin_cli:insert(publish, [From, F2(Username),
                                            io_lib:format("~B.~B.~B.~B",[B1, B2, B3, B4]),
                                            I1 + I2 + I3, F1(Dup),
                                            F1(Retain),
                                            Qos,
                                            Topic,
                                            Payload,
                                            Ts]),
    {ok, Message};
% point to point
on_message_publish(Message = #message{topic = <<"$P2P/", PeerClientId/binary>>,
                                      qos = QOS,
                                      payload = Payload,
                                      from = From}, _Env) ->
    case erlang:byte_size(PeerClientId) > 0 of
            true ->
                    case rpc:multicall([node() | nodes()], ets,lookup, [emqx_channel, PeerClientId]) of
                        {[[{_, ChannelPid}] | _] ,_} ->
                                P2PMessage = emqx_message:make(From, QOS, <<"$P2P/", PeerClientId/binary >> , Payload),
                                ChannelPid ! {deliver, <<"$P2P/", PeerClientId/binary>>, P2PMessage},
                                {ok, Message};
                        _ ->
                            {stop, deny}
                    end;
             _ ->
                {stop, deny}
    end;

on_message_publish(Message , _Env) ->
    {ok, Message}.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
parse_rule(Rules) ->
    parse_rule(Rules, []).
parse_rule([], Acc) ->
    lists:reverse(Acc);
parse_rule([{Rule, Conf} | Rules], Acc) ->
    Params = emqx_json:decode(iolist_to_binary(Conf)),
    Action = proplists:get_value(<<"action">>, Params),
    Filter = proplists:get_value(<<"topic">>, Params),
    parse_rule(Rules, [{list_to_atom(Rule), Action, Filter} | Acc]).

with_filter(Fun, _, undefined) ->
    Fun(), ok;
with_filter(Fun, Topic, Filter) ->
    case emqx_topic:match(Topic, Filter) of
        true  -> Fun(), ok;
        false -> ok
    end.

load_(Hook, _Fun, Params) ->
    case Hook of
        'client.connected'    -> emqx:hook(Hook, fun emqx_persistence_plugin:on_client_connected/3, [Params]);
        'client.disconnected' -> emqx:hook(Hook, fun emqx_persistence_plugin:on_client_disconnected/4, [Params]);
        'client.subscribe'    -> emqx:hook(Hook, fun emqx_persistence_plugin:on_client_subscribe/4, [Params]);
        'client.unsubscribe'  -> emqx:hook(Hook, fun emqx_persistence_plugin:on_client_unsubscribe/4, [Params]);
        'message.publish'     -> emqx:hook(Hook, fun emqx_persistence_plugin:on_message_publish/2, [Params])
    end.

unload_(Hook, _Fun) ->
    case Hook of
        'client.connected'    -> emqx:unhook(Hook, fun emqx_persistence_plugin:on_client_connected/3);
        'client.disconnected' -> emqx:unhook(Hook, fun emqx_persistence_plugin:on_client_disconnected/4);
        'client.subscribe'    -> emqx:unhook(Hook, fun emqx_persistence_plugin:on_client_subscribe/4);
        'client.unsubscribe'  -> emqx:unhook(Hook, fun emqx_persistence_plugin:on_client_unsubscribe/4);
        'message.publish'     -> emqx:unhook(Hook, fun emqx_persistence_plugin:on_message_publish/2)
    end.
