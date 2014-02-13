-module(kvs_subscription).
-copyright('Synrc Research Center s.r.o.').
-include("subscription.hrl").
-include("state.hrl").
-include("user.hrl").
-include("config.hrl").
-include("kvs.hrl").
-include("metainfo.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=subscription,fields=record_info(fields,subscription),keys=[whom,who]}
    ]}.

subscribe(Who, Whom) ->
    Record = #subscription{who = Who, whom = Whom},
    kvs:put(Record).

unsubscribe(Who, Whom) ->
    case subscribed(Who, Whom) of
        true  -> kvs:delete(subscription, {Who, Whom});
        false -> skip end.

subscriptions(undefined)-> [];
subscriptions(#user{username = UId}) -> subscriptions(UId);

subscriptions(UId) -> DBA=?DBA, DBA:subscriptions(UId).
subscribed(Who) -> DBA=?DBA, DBA:subscribed(Who).

subscribed(Who, Whom) ->
    case kvs:get(subscription, {Who, Whom}) of
        {ok, _} -> true;
        _ -> false end.
