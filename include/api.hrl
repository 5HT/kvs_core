-include("metainfo.hrl").

% service

-spec start() -> ok | {error,any()}.
-spec stop() -> stopped.

% schema change

-spec destroy() -> ok.
-spec join() -> ok | {error,any()}.
-spec join(Node :: string()) -> [{atom(),any()}].
-spec init_db() -> list(tuple(list(), skip | ok)).
-spec init(Backend :: atom(),Module :: atom()) -> list(#table{}).

% meta info

-spec modules() -> list(atom()).
-spec containers() -> list(tuple(atom(),list(atom()))).
-spec tables() -> list(#table{}).
-spec table(Table :: atom()) -> #table{}.
-spec version() -> {version,string()}.

% chain ops

-spec add(Record :: tuple()) -> {ok,tuple()} | {error,exist} | {error,no_container}.
-spec remove(Record :: tuple()) -> ok | {error,any()}.
-spec remove(Record :: atom(), Id :: any()) -> ok | {error,any()}.
