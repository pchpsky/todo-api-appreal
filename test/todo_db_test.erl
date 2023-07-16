-module(todo_db_test).

-include_lib("eunit/include/eunit.hrl").

-include("include/todo.hrl").

-define(TODO_PROPS, #todo_props{completed = false, title = "Test Todo"}).

setup() ->
  application:set_env(mnesia, dir, "./test/db"),
  ok = todo_db:init().

cleanup(_Config) ->
  mnesia:stop(),
  mnesia:delete_schema([node()]).

todo_db_test_() ->
  {setup,
   fun setup/0,
   fun cleanup/1,
   [{"todo_db:all/0", ?_test(test_all())},
    {"todo_db:find/1", ?_test(test_find())},
    {"todo_db:update/2", ?_test(test_update())},
    {"todo_db:delete/1", ?_test(test_delete())}]}.

test_all() ->
  ?assertMatch([], todo_db:all()),
  {ok, Todo} = todo_db:insert(?TODO_PROPS),
  ?assertMatch([Todo], todo_db:all()).

test_find() ->
  ?assertMatch({error, not_found}, todo_db:find(-1)),
  {ok, Todo} = todo_db:insert(?TODO_PROPS),
  ?assertMatch({ok, Todo}, todo_db:find(Todo#todo.id)).

test_update() ->
  {ok, #todo{id = TodoID}} = todo_db:insert(?TODO_PROPS),

  ?assertMatch({ok, #todo{title = "New Title"}},
               todo_db:update(TodoID, fun(Todo) -> Todo#todo{title = "New Title"} end)),

  ?assertMatch({ok, #todo{completed = true}},
               todo_db:update(TodoID, fun(Todo) -> Todo#todo{completed = true} end)),

  ?assertMatch({ok, #todo{title = "New Title", completed = true}},
               todo_db:update(TodoID,
                              fun(Todo) -> Todo#todo{title = "New Title", completed = true} end)),

  ?assertMatch({error, not_found}, todo_db:update(-1, ?TODO_PROPS)).

test_delete() ->
  {ok, #todo{id = TodoID}} = todo_db:insert(?TODO_PROPS),
  ?assertMatch(ok, todo_db:delete(TodoID)),
  ?assertMatch({error, not_found}, todo_db:find(TodoID)).
