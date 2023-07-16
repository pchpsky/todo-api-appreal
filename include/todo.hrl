-record(todo, {
    title :: string(),
    completed :: boolean()
}).

-record(persisted_todo, {
    id :: integer(),
    title :: string(),
    completed :: boolean()
}).
