-include("kvs.hrl").

-record(comment, {?ITERATOR(feed), % {comment_id, entry_id, feed_id}
        comment_id,
        entry_id,
        content,
        from,
        created,
        media = [],
        parent}).
