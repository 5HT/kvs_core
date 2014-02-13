-module(kvs).
-author('Synrc Research Center s.r.o.').
-include("config.hrl").
-include("metainfo.hrl").
-include("state.hrl").
-include("kvs.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

start() -> DBA = ?DBA, DBA:start().
dir() -> DBA = ?DBA, DBA:dir().
stop() -> DBA = ?DBA, DBA:stop().
initialize() -> DBA = ?DBA, DBA:initialize().
delete() -> DBA = ?DBA, DBA:delete().
join() -> DBA = ?DBA, DBA:join().
join(Node) -> DBA = ?DBA, DBA:join(Node).

modules() ->
    Modules = case kvs:config(schema) of
        [] -> [ kvs_user, kvs_feed, kvs_acl ];
        E  -> E end.

containers() ->
    [ [ {T#table.name,T#table.fields}
        || T=#table{container=true} <- (M:metainfo())#schema.tables ]
    || M <- modules() ].

tables() -> lists:flatten([ (M:metainfo())#schema.tables || M <- modules() ]).

table(Name) -> lists:keyfind(Name,#table.name,tables()).

init(Backend, Module) ->
    [ begin
        store_mnesia:create_table(T#table.name, T#table.fields, [{storage, permanent}]),
        [ Backend:add_table_index(T#table.name, Key) || Key <- T#table.keys ]
    end || T <- (Module:metainfo())#schema.tables ].

create(ContainerName) ->
    Id = kvs:next_id(atom_to_list(ContainerName), 1),
    Instance = list_to_tuple([ContainerName|proplists:get_value(ContainerName, ?CONTAINERS)]),
    Top = setelement(#container.id,Instance,Id),
    ok = kvs:put(Top),
    Id.

add(Record) when is_tuple(Record) ->

    Id = element(#iterator.id, Record),

    case kvs:get(element(1,Record), Id) of
        {ok, _} -> error_logger:info_msg("Entry exist: ~p", [Id]),{error, exist};
        {error, not_found} ->

            Type = element(1, Record),
            CName = element(#iterator.container, Record),
            Cid = case element(#iterator.feed_id, Record) of
                undefined -> element(1,Record); %?FEED(Type);
                Fid -> Fid end,

            Container = case kvs:get(CName, Cid) of
                {ok,C} -> C;
                {error, not_found} when Cid /= undefined ->

                    NC = setelement(#container.id,list_to_tuple([CName|proplists:get_value(CName, ?CONTAINERS)]), Cid),
                    NC1 = setelement(#container.entries_count, NC, 0),

                    kvs:put(NC1),NC1;

                _ -> error end,

            if  Container == error -> {error, no_container};
                true ->

                    Next = undefined,
                    Prev = case element(#container.top, Container) of
                        undefined -> undefined;
                        Tid -> case kvs:get(Type, Tid) of
                            {error, not_found} -> undefined;
                            {ok, Top} ->
                                NewTop = setelement(#iterator.next, Top, Id),
                                kvs:put(NewTop),
                                element(#iterator.id, NewTop) end end,

                    C1 = setelement(#container.top, Container, Id),
                    C2 = setelement(#container.entries_count, C1, element(#container.entries_count, Container)+1),

                    kvs:put(C2),

                    R  = setelement(#iterator.feeds, Record,
                            [ case F1 of
                                {FN, Fd} -> {FN, Fd};
                                _-> {F1, kvs:create(CName)}
                              end || F1 <- element(#iterator.feeds, Record)]),

                    R1 = setelement(#iterator.next,    R,  Next),
                    R2 = setelement(#iterator.prev,    R1, Prev),
                    R3 = setelement(#iterator.feed_id, R2, element(#container.id, Container)),

                    kvs:put(R3),

                    error_logger:info_msg("[kvs] PUT: ~p", [element(#container.id,R3)]),

                    {ok, R3}
            end;
        E ->  error_logger:info_msg("Entry exist: ~p", [E]),{error, exist} end.

remove(RecordName, RecordId) ->
    case kvs:get(RecordName, RecordId) of
        {error, not_found} -> error_logger:info_msg("not found");
        {ok, E} ->

            Id = element(#iterator.id, E),
            CName = element(#iterator.container, E),
            Cid = element(#iterator.feed_id, E),

            {ok, Container} = kvs:get(CName, Cid),
            Top = element(#container.top, Container),

            Next = element(#iterator.next, E),
            Prev = element(#iterator.prev, E),

            case kvs:get(RecordName, Next) of
                {ok, NE} ->
                    NewNext = setelement(#iterator.prev, NE, Prev),
                    kvs:put(NewNext);
                    _ -> ok end,

            case kvs:get(RecordName, Prev) of
                {ok, PE} ->
                    NewPrev = setelement(#iterator.next, PE, Next),
                    kvs:put(NewPrev);
                    _ -> ok end,

            C1 = case Top of Id -> setelement(#container.top, Container, Prev); _ -> Container end,
            C2 = setelement(#container.entries_count, C1, element(#container.entries_count, Container)-1),

            kvs:put(C2),

            error_logger:info_msg("[kvs] DELETE: ~p id: ~p", [RecordName, Id]),

            kvs:delete(RecordName, Id) end.

remove(E) when is_tuple(E) ->

    Id    = element(#iterator.id, E),
    CName = element(#iterator.container, E),
    Cid   = element(#iterator.feed_id, E),

    {ok, Container} = kvs:get(CName, Cid),

    Top   = element(#container.top, Container),
    Next  = element(#iterator.next, E),
    Prev  = element(#iterator.prev, E),

    case kvs:get(element(1,E), Next) of
        {ok, NE} ->
            NewNext = setelement(#iterator.prev, NE, Prev),
            kvs:put(NewNext); _ -> ok end,

    case kvs:get(element(1,E), Prev) of
        {ok, PE} ->
            NewPrev = setelement(#iterator.next, PE, Next),
            kvs:put(NewPrev);
        _ -> ok end,

    C1 = case Top of Id -> setelement(#container.top, Container, Prev); _ -> Container end,
    C2 = setelement(#container.entries_count, C1, element(#container.entries_count, Container)-1),

    kvs:put(C2),

    error_logger:info_msg("[kvs] DELETE: ~p", [Id]),

    kvs:delete(E).

traversal( _,undefined,_,_) -> [];
traversal(_,_,0,_) -> [];
traversal(RecordType, Start, Count, Direction)->
    case kvs:get(RecordType, Start) of {error,_} -> [];
    {ok, R} ->  Prev = element(Direction, R),
                Count1 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
                [R | traversal(RecordType, Prev, Count1, Direction)] end.

entries(RecordType, Count) -> 
    Name = element(1,RecordType),
    Table = kvs:table(Name),
    entries(kvs:get(Table#table.container,Name), RecordType, Count).

entries({ok, Container}, RecordType, Count) -> entries(Container, RecordType, Count);
entries(Container, RecordType, Count) when is_tuple(Container) -> traversal(RecordType, element(#container.top, Container), Count, #iterator.prev);
entries(_,_,_) -> error_logger:info_msg("=> ENTRIES ARGS NOT MATCH!"), [].

entries(RecordType, Start, Count, Direction) ->
    E = traversal(RecordType, Start, Count, Direction),
    case Direction of #iterator.next -> lists:reverse(E); #iterator.prev -> E end.

init_db() ->
    case kvs:get(id_seq,"feed") of
        {error,_} -> add_seq_ids();
        {ok,_} -> ignore end.

add_seq_ids() ->
    Init = fun(Key) ->
           case kvs:get(id_seq, Key) of
                {error, _} -> ok = kvs:put(#id_seq{thing = Key, id = 0});
                {ok, _} -> ignore
           end
    end,
    [ Init(atom_to_list(Name)) || {Name,Fields} <- containers() ].

version() -> DBA=?DBA, DBA:version().

put(Record) ->
    DBA=?DBA,
    DBA:put(Record).

get(RecordName, Key) ->
    DBA=?DBA,
    DBA:get(RecordName, Key).

get(RecordName, Key, Default) ->
    DBA=?DBA,
    case DBA:get(RecordName, Key) of
        {ok,{RecordName,Key,Value}} ->
            error_logger:info_msg("db:get config value ~p,", [{RecordName, Key, Value}]),
            {ok,Value};
        {error, _B} ->
            error_logger:info_msg("db:get new config value ~p,", [{RecordName, Key, Default}]),
            DBA:put({RecordName,Key,Default}),
            {ok,Default} end.

delete(Keys) -> DBA=?DBA, DBA:delete(Keys).
delete(Tab, Key) -> DBA=?DBA,DBA:delete(Tab, Key).
delete_by_index(Tab, IndexId, IndexVal) -> DBA=?DBA,DBA:delete_by_index(Tab, IndexId, IndexVal).
multi_select(RecordName, Keys) -> DBA=?DBA,DBA:multi_select(RecordName, Keys).
select(From, PredicateFunction) -> DBA=?DBA, DBA:select(From, PredicateFunction).
count(RecordName) -> DBA=?DBA,DBA:count(RecordName).
all(RecordName) -> DBA=?DBA,DBA:all(RecordName).
all_by_index(RecordName, Index, IndexValue) -> DBA=?DBA,DBA:all_by_index(RecordName, Index, IndexValue).
next_id(RecordName) -> DBA=?DBA,DBA:next_id(RecordName).
next_id(RecordName, Incr) -> DBA=?DBA,DBA:next_id(RecordName, Incr).
next_id(RecordName, Default, Incr) -> DBA=?DBA,DBA:next_id(RecordName, Default, Incr).

save_db(Path) ->
    Data = lists:append([all(B) || B <- [list_to_atom(Name) || {table,Name} <- kvs:dir()] ]),
    kvs:save(Path, Data).

load_db(Path) ->
    add_seq_ids(),
    AllEntries = kvs:load(Path),
    [case is_tuple(E) of
        false -> skip;
        true ->  kvs:put(E) 
    end || E <- AllEntries].

save(Dir, Value) ->
    filelib:ensure_dir(Dir),
    file:write_file(Dir, term_to_binary(Value)).

load(Key) ->
    {ok, Bin} = file:read_file(Key),
    binary_to_term(Bin).

coalesce(undefined, B) -> B;
coalesce(A, _) -> A.

uuid() ->
  R1 = random:uniform(round(math:pow(2, 48))) - 1,
  R2 = random:uniform(round(math:pow(2, 12))) - 1,
  R3 = random:uniform(round(math:pow(2, 32))) - 1,
  R4 = random:uniform(round(math:pow(2, 30))) - 1,
  R5 = erlang:phash({node(), now()}, round(math:pow(2, 32))),

  UUIDBin = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>,
  <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = UUIDBin,

  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b-~8.16.0b",
                              [TL, TM, THV, CSR, CSL, N, R5])).
uuname() ->
  lists:flatten(io_lib:format("~8.16.0b",[erlang:phash2({node(), now()}, round(math:pow(2, 32)))])).

sha(Raw) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || <<N>> <= crypto:sha(Raw)]).

sha_upper(Raw) ->
    SHA = sha(Raw),
    string:to_upper(SHA).

config(Key) -> config(kvs, Key, "").
config(App,Key) -> config(App,Key, "").
config(App, Key, Default) -> case application:get_env(App,Key) of
                              undefined -> Default;
                              {ok,V} -> V end.

