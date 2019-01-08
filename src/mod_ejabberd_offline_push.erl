%%%-------------------------------------------------------------------
%%% @author jitendradixit
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2018 12:24 PM
%%%-------------------------------------------------------------------
-module(mod_ejabberd_offline_push).
-author("dev@codepond.org").


-export([start/2, stop/1, store_packet/1,muc_filter_message/3]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("mod_muc_room.hrl").



start(_Host, _Opt) ->
  inets:start(),
  ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, store_packet, 50),
  ejabberd_hooks:add(muc_filter_message, _Host, ?MODULE, muc_filter_message, 50).

stop(_Host) ->
  ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, store_packet, 50),
  ejabberd_hooks:delete(muc_filter_message, _Host, ?MODULE, muc_filter_message, 50).


store_packet({_Action, #message{from = From, to = To} = Packet}) ->
  case Packet#message.body /= [] of
    true ->
      [{text, _, Body}] = Packet#message.body,
      post_offline_message(From, To, Body, Packet#message.id)
  end.

muc_filter_message(#message{from = From} = Packet,
       #state{config = Config, jid = RoomJID} = MUCState,
       FromNick) ->
    [{text, _, Body}] = Packet#message.body,

    _LISTUSERS = lists:map(
        fun({_LJID, Info}) ->
            binary_to_list(Info#user.jid#jid.luser) ++ ".."
        end,
        dict:to_list(MUCState#state.users)
    ),
    ?DEBUG(" #########    GROUPCHAT _LISTUSERS = ~p~n  #######   ", [_LISTUSERS]),

    _AFILLIATIONS = lists:map(
        fun({{Uname, _Domain, _Res}, _Stuff}) ->
            binary_to_list(Uname) ++ ".."
        end,
        dict:to_list(MUCState#state.affiliations)
    ),
    ?DEBUG(" #########    GROUPCHAT _AFILLIATIONS = ~p~n  #######   ", [_AFILLIATIONS]),

    _OFFLINE = lists:subtract(_AFILLIATIONS, _LISTUSERS),
    ?DEBUG(" #########    GROUPCHAT _OFFLINE = ~p~n  #######   ", [_OFFLINE]),

    Roomname = MUCState#state.room,
    Subject = MUCState#state.subject,
    Config = MUCState#state.config,
    Title = Config#config.title,
    ?INFO_MSG("Room Name: ~p, Title: ~p,FromNick: ~p,Subject: ~p.", [Roomname, Title, FromNick,Subject]),

    if
        Body /= "", length(_OFFLINE) > 0 ->
            ToUser = binary_to_list(RoomJID#jid.luser),
            FromUser = binary_to_list(From#jid.luser),
            FinalBody = binary_to_list(Body),
            MessageId = binary_to_list(Packet#message.id),
            PostUrl = gen_mod:get_module_opt(RoomJID#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
            DataBody = "{\"toJID\":\"" ++ ToUser ++ "\",\"fromJID\":\"" ++ FromUser ++ "\",\"body\":\"" ++ FinalBody ++ "\",\"messageID\":\"" ++ MessageId ++ "\"}",
            Method = post,
            URL = binary_to_list(PostUrl),
            Header = [],
            Type = "application/json",
            HTTPOptions = [],
            Options = [],
            %%  inets:start(),
            %%  ssl:start(),
            R = httpc:request(Method, {URL, Header, Type, DataBody}, HTTPOptions, Options),
            io:fwrite(R),
            Packet;
        true ->
            Packet
    end.  

post_offline_message(From, To, Body, MsgId) ->
  ToUser = binary_to_list(To#jid.luser),
  FromUser = binary_to_list(From#jid.luser),
  FinalBody = binary_to_list(Body),
  MessageId = binary_to_list(MsgId),
  PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  DataBody = "{\"toJID\":\"" ++ ToUser ++ "\",\"fromJID\":\"" ++ FromUser ++ "\",\"body\":\"" ++ FinalBody ++ "\",\"messageID\":\"" ++ MessageId ++ "\"}",
  Method = post,
  URL = binary_to_list(PostUrl),
  Header = [],
  Type = "application/json",
  HTTPOptions = [],
  Options = [],
  %%  inets:start(),
  %%  ssl:start(),
  R = httpc:request(Method, {URL, Header, Type, DataBody}, HTTPOptions, Options),
  io:fwrite(R).


