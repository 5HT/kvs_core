-include("kvs.hrl").

-record(user, {?ITERATOR(feed, true),
        email,
        username,
        password,
        tokens,
        avatar,
        names,
        surnames,
        birth,
        sex,
        date,
        status,
        zone,
        type }).
