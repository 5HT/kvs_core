-ifndef(KVS_HRL).
-define(KVS_HRL, true).

-record(id_seq, {thing, id}).
-define(CONTAINER, id, top, entries_count=0).
-define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard).
-define(ITERATOR(Container), ?ITERATOR(Container, false)).
-record(container, {?CONTAINER}).
-record(iterator,  {?ITERATOR(undefined)}).
-define(CREATE_TAB(T), store_mnesia:create_table(T, record_info(fields, T), [{storage, permanent}]) ).
-define(CONTAINERS, kvs:containers()).

-endif.
