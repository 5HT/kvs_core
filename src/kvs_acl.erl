-module(kvs_acl).
-copyright('Synrc Research Center s.r.o.').
-compile(export_all).
-include("kvs.hrl").
-include("metainfo.hrl").
-include("acl.hrl").
-include("user.hrl").
-include("group.hrl").
-include("feed.hrl").

metainfo() -> 
    #schema{name=kvs,tables=[
        #table{name=acl,container=true,fields=record_info(fields,acl)},
        #table{name=access,container=acl,fields=record_info(fields,access)}
    ]}.

define_access(Accessor, Resource, Action) -> 
    Entry = #access{ id={Accessor, Resource}, accessor=Accessor, action=Action, feed_id=Resource},
    case kvs:add(Entry) of {error, exist} -> kvs:put(Entry#access{action=Action}); {ok, E} -> E end.

check(Keys) ->
    Acls = [Acl || {ok, Acl = #access{}} <- [kvs:get(access, Key) || Key <- Keys]],
    case Acls of [] -> none;
        [#access{action = Action} | _] -> Action end.

check_access(#user{id = UId, type = UType}, #feed{id = FId}) ->
    Feed = {feed, FId},
    Query = [ {{user, UId}, Feed}, {{user_type, UType}, Feed}, {default, Feed}],
    check(Query);

check_access(#user{id = UId, type = UType}, #group{id = GId}) ->
    Group = {group, GId},
    Query = [ {{user, UId}, Group}, {{user_type, UType}, Group}, {default, Group}],
    check(Query);

check_access(#user{id = AId, type = AType}, #user{id = RId}) ->
    User = {user, RId},
    Query = [ {{user, AId}, User}, {{user_type, AType}, User}, {default, User} ],
    check(Query);

check_access({user_type, Type}, #user{id = RId}) ->
    User = {user, RId},
    Query = [ {{user_type, Type}, User}, {default, User} ],
    check(Query);

check_access({user_type, Type}, #feed{id = FId}) ->
    Feed = {feed, FId},
    Query = [ {{user_type, Type}, Feed}, {default, Feed} ],
    check(Query);

check_access({user_type, Type}, #group{id = GId}) ->
    Group = {group, GId},
    Query = [{{user_type, Type}, Group}, {default, Group}],
    check(Query);

check_access(#user{id = AId, type = AType}, {feature, _Feature} = R) ->
    Query = [ {{user, AId}, R}, {{user_type, AType}, R}, {default, R} ],
    check(Query);

check_access(UId, {feature, _Feature} = Resource) ->
    case kvs:get(user, UId) of
        {ok, User} -> check_access(User, Resource);
        E -> E end.
