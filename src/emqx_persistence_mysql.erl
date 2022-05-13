-module(emqx_persistence_mysql).

-include("emqx_persistence_mysql.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(PERSISTENCE_KEY, "$MYSQL").

-export([ load/1,
          unload/0]).
-export([ on_client_connected/3,
          on_client_disconnected/4,
          on_client_subscribe/4
        ]).
-export([ on_message_publish/2]).

load(Env) ->
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}).

unload() ->
    emqx:unhook('message.publish',     {?MODULE, on_message_publish}),
    emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}).

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------

on_client_subscribe(#{clientid := _ClientId, username := _Username}, _Properties, RawTopicFilters, _) ->
    lists:foreach(fun({Topic, _Opts}) ->
        case string:left(erlang:binary_to_list(Topic), erlang:length(?PERSISTENCE_KEY)) of
            ?PERSISTENCE_KEY ->
                {stop, deny};
            _ ->
                {matched, allow}
        end
    end, RawTopicFilters).
%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(#{clientid := ClientId,
                      username := Username,
                      peerhost := Peerhost}, ConnInfo, _Env) ->
    Action = <<"client_connected">>,
    Node = erlang:atom_to_binary(node()),
    Ipaddress = iolist_to_binary(inet:ntoa(Peerhost)),
    ConnectedAt = maps:get(connected_at, ConnInfo),
    Data = [Action, Node, stringfy(ClientId), stringfy(Username),
            Ipaddress, ConnectedAt],
    emqx_persistence_mysql_cli:insert(connected, Data),
    ok.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
on_client_disconnected(#{clientid := ClientId,
                         username := Username}, Reason, ConnInfo, _Env) ->
    Action = <<"client_disconnected">>,
    Node = erlang:atom_to_binary(node()),
    DisconnectedAt = maps:get(disconnected_at, ConnInfo, erlang:system_time(millisecond)),
    Data = [Action, Node, stringfy(ClientId), stringfy(Username), stringfy(Reason), DisconnectedAt],
    emqx_persistence_mysql_cli:insert(disconnected, Data),
    ok.
%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message = #message{topic = <<?PERSISTENCE_KEY, _/binary>> = _Topic}, _Env) ->
    {FromClientId, FromUsername} = parse_from(Message),
    Action = <<"message_publish">>,
    Node = erlang:atom_to_binary(node()),
    Topic = Message#message.topic,
    MsgId = emqx_guid:to_hexstr(Message#message.id),
    Payload = Message#message.payload,
    Ts = Message#message.timestamp,
    Data = [Action, Node, stringfy(FromClientId),
                          stringfy(FromUsername), Topic, MsgId, Payload, Ts],
    emqx_persistence_mysql_cli:insert(publish, Data),
    {ok, Message};

on_message_publish(Message , _Env) ->
    {ok, Message}.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

maybe(undefined) -> null;

maybe(Str) -> Str.

stringfy(Term) when is_binary(Term) ->
    Term;
stringfy(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
stringfy(Term) ->
    unicode:characters_to_binary((io_lib:format("~0p", [Term]))).
parse_from(Message) ->
    {emqx_message:from(Message), maybe(emqx_message:get_header(username, Message))}.
