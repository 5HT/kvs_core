-ifndef(METAINFO_HRL).
-define(METAINFO_HRL, true).

-record(schema,{name,tables=[]}).
-record(table,{name,container,fields=[],keys=[]}).

-record(meta,{?CONTAINER}).
-record(document,{?ITERATOR(meta),sections,fields,access}).
-record(section,{fields}).
-record(field,{id,section,name,title,layout,desc,type,etc,access}).

-endif.
