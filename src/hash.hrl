-define(DEFAULT_CAPACITY, 16).

-record(hash_set, {size = 0, capacity = 16, table = []}).
