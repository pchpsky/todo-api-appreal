-record(todo_props, {
    title :: string(),
    completed = false :: boolean()
}).

-record(todo, {
    id :: integer(),
    title :: string(),
    completed :: boolean()
}).
