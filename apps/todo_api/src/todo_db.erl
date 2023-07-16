-module(todo_db).

-include("include/todo.hrl").

-export([all/0, find/1, insert/1, update/2, delete/1, init/0]).

all() ->
  mnesia:activity(transaction, fun() -> mnesia:match_object({todo, '_', '_', '_'}) end).

find(TodoID) ->
  case mnesia:activity(transaction, fun() -> mnesia:read({todo, TodoID}) end) of
    [Todo] ->
      {ok, Todo};
    [] ->
      {error, not_found}
  end.

insert(TodoProps = #todo_props{}) ->
  Todo =
    #todo{id = next_todo_id(),
          title = TodoProps#todo_props.title,
          completed = TodoProps#todo_props.completed},
  case mnesia:transaction(fun() -> mnesia:write(Todo) end) of
    {atomic, ok} ->
      {ok, Todo};
    {aborted, _} ->
      error
  end.

update(TodoID, Fun) ->
  case find(TodoID) of
    {ok, Todo} ->
      TodoProps = todo_props(Fun(Todo)),
      NewTodo =
        Todo#todo{title = TodoProps#todo_props.title, completed = TodoProps#todo_props.completed},
      case mnesia:transaction(fun() -> mnesia:write(NewTodo) end) of
        {atomic, ok} ->
          {ok, NewTodo};
        {aborted, _} ->
          {error, failed_to_update}
      end;
    {error, not_found} ->
      {error, not_found}
  end.

delete(TodoID) ->
  case find(TodoID) of
    {ok, _} ->
      case mnesia:transaction(fun() -> mnesia:delete({todo, TodoID}) end) of
        {atomic, ok} ->
          ok;
        {aborted, Reason} ->
          {error, Reason}
      end;
    {error, not_found} ->
      {error, not_found}
  end.

init() ->
  mnesia:create_schema([node()]),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  mnesia:start(),
  mnesia:create_table(todo,
                      [{attributes, record_info(fields, todo)}, {disc_copies, [node()]}]),
  mnesia:create_table(todo_id, [{disc_copies, [node()]}]),
  mnesia:wait_for_tables([todo, todo_id], 5000).

todo_props(#todo_props{} = Todo) ->
  Todo;
todo_props(#todo{} = Todo) ->
  #todo_props{title = Todo#todo.title, completed = Todo#todo.completed}.

next_todo_id() ->
  mnesia:dirty_update_counter(todo_id, 2, 1).
