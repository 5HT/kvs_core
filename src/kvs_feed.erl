-module(kvs_feed).
-copyright('Synrc Research Center, s.r.o.').
-compile(export_all).
-include("kvs.hrl").
-include("entry.hrl").
-include("feed.hrl").
-include("metainfo.hrl").
-include("comment.hrl").
-include("state.hrl").
-define(CACHED_ENTRIES, 20).

metainfo() -> 
    #schema{name=kvs,tables=[
        #table{name=entry,container=feed,fields=record_info(fields,entry),keys=[feed_id,entry_id,from]},
        #table{name=comment,container=feed,fields=record_info(fields,comment),keys=[entry_id,author_id]},
        #table{name=feed,container=true,fields=record_info(fields,feed)}
    ]}.

comments_count(entry, Eid) -> case kvs:get(entry, Eid) of {error,_} -> 0; {ok, E} -> comments_count([E],0) end;
comments_count(product, Pid)->case kvs:get(product, Pid) of {error,_}->0; {ok, P} -> comments_count([P], 0) end;
comments_count([], Acc) -> Acc;
comments_count([E|T], Acc) ->
    C = case lists:keyfind(comments, 1, element(#iterator.feeds, E)) of false -> 0;
    {_, Fid} -> case kvs:get(feed, Fid) of {error,_} -> 0;
        {ok, Feed } -> Feed#feed.entries_count 
            + comments_count(kvs:entries(Feed, comment, undefined), 0) end end,
    comments_count(T,  C + Acc).

author_comments(Who) ->
    EIDs = [E || #comment{entry_id=E} <- kvs:index(comment,from, Who) ],
    lists:flatten([ kvs:index(entry, id,EID) || EID <- EIDs]).

%% MQ API

handle_notice([kvs_feed, _, Owner, entry, Eid, add],
              [#entry{feed_id=Fid}=Entry],
              #state{owner=Owner} = S) ->
    case lists:keyfind(Fid,2, S#state.feeds) of false -> skip;
    {_,_} -> add_entry(Eid,Fid,Entry) end,

    {noreply, S};

handle_notice([kvs_feed,_, Owner, entry, {Eid, FeedName}, edit],
              [#entry{}=Entry],
              #state{owner=Owner, feeds=Feeds}=S) ->
    case lists:keyfind(FeedName,1,Feeds) of false -> skip; {_,Fid}-> update_entry(Eid,Fid,Entry) end,

    {noreply, S};

handle_notice([kvs_feed,_, Owner, entry, Eid, edit],
              [#entry{feed_id=Fid}=Entry],
              #state{owner=Owner, feeds=Feeds}=S) ->
    case lists:keyfind(Fid, 2, Feeds) of false -> skip;
    {_,_} -> update_entry(Eid,Fid,Entry) end,

    {noreply, S};

handle_notice([kvs_feed, Owner, entry, delete],
              [#entry{id=Id,feed_id=Fid}=E],
              #state{owner=Owner, feeds=Feeds}=State) ->
    error_logger:info_msg("DELETE"),
    case lists:keyfind(Fid,2,Feeds) of false -> ok;
    _ ->
        error_logger:info_msg("[kvs_feed] => Remove entry ~p from feed ~p", [Id, Fid]),
        kvs:remove(entry, Id),
        msg:notify([kvs_feed, entry, Id, deleted], [E]) end,

    {noreply,State};

handle_notice([kvs_feed, Owner, delete],
              [#entry{entry_id=Eid}=E],
              #state{owner=Owner}=State) ->
    error_logger:info_msg("[kvs_feed] Delete all entries ~p ~p", [E#entry.entry_id, Owner]),

    [msg:notify([kvs_feed, To, entry, delete],[Ed])
        || #entry{to={_, To}}=Ed <- kvs:all_by_index(entry, entry_id, Eid)],

    Fid = element(1,E),
    kvs:remove(entry,{Eid, Fid}),
    Removed = E#entry{id={Eid,Fid},feed_id=Fid},
    msg:notify([kvs_feed, entry, {Eid, Fid}, deleted], [Removed]),

    {noreply, State};

handle_notice([kvs_feed,_,Owner, comment, Cid, add],
              [#comment{id={Cid, {_, EFid}, _}}=C],
              #state{owner=Owner, feeds=Feeds} = S) ->
    case lists:keyfind(EFid,2,Feeds) of false -> skip;
    {_,_}-> add_comment(C) end,

    {noreply, S};

handle_notice(_Route, _Message, State) ->
  %error_logger:error_msg("~p ===> Unknown FEED notice ~p", [State#state.owner, Route]), 
  {noreply, State}.

add_comment(C) ->
    error_logger:info_msg("[kvs_feed] Add comment ~p ~p", [C#comment.id, C#comment.feed_id]),
    Added = case kvs:add(C#comment{feeds=[comments]}) of {error, E} -> {error, E}; {ok, Cm} -> Cm end,
    msg:notify([kvs_feed, comment, C#comment.id, added], [Added]).

add_entry(Eid,Fid,Entry) ->
    error_logger:info_msg("[kvs_feed] => Add entry ~p to feed ~p.", [Eid, Fid]),
    E = Entry#entry{id = {Eid, Fid}, entry_id = Eid, feeds=[comments]},
    Added = case kvs:add(E) of {error, Err}-> {error,Err}; {ok, En} -> En end,
    msg:notify([kvs_feed, entry, {Eid, Fid}, added], [Added]).

update_entry(Eid,Fid,Entry) ->
    case kvs:get(entry, {Eid,Fid}) of
    {error,_} -> skip;
    {ok, E} ->
        error_logger:info_msg("[kvs_feed] => Update entry ~p in ~p", [Eid, Fid]),
        Upd = E#entry{description=Entry#entry.description,
                      title = Entry#entry.title,
                      media = Entry#entry.media,
                      etc   = Entry#entry.etc,
                      type  = Entry#entry.type},
        kvs:put(Upd),
        msg:notify([kvs_feed, entry, {Eid, Fid}, updated], [Upd]) end.
