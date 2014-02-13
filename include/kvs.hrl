-ifndef(KVS_HRL).
-define(KVS_HRL, true).

-record(id_seq, {thing, id}).
-define(CONTAINER, id, top, entries_count=0).
-define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard).
-define(ITERATOR(Container), ?ITERATOR(Container, false)).
-record(container, {?CONTAINER}).
-record(iterator,  {?ITERATOR(undefined)}).
-define(CONTAINERS, kvs:containers()).

-endif.
