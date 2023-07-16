-module(todo_db).

-include("include/todo.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([all/0, get/1, create/1, update/2, delete/1]).

-spec all() -> [#persisted_todo{}].
all() ->
  gen_server:call(?MODULE, all).

-spec get(integer()) -> {ok, #persisted_todo{}} | error.
get(TodoID) ->
  gen_server:call(?MODULE, {get, TodoID}).

-spec create(#todo{}) -> {ok, #persisted_todo{}} | error.
create(Todo = #todo{}) ->
  gen_server:call(?MODULE, {create, Todo}).

-spec update(integer(), #todo{}) -> {ok, #persisted_todo{}} | error.
update(TodoID, Todo = #todo{}) ->
  gen_server:call(?MODULE, {update, TodoID, Todo}).

-spec delete(integer()) -> ok.
delete(TodoID) ->
  gen_server:call(?MODULE, {delete, TodoID}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #{todos => maps:new(), next_todo_id => 1}}.

handle_call(all, _From, #{todos := Todos} = State) ->
  {reply, maps:values(Todos), State};
handle_call({get, TodoID}, _From, #{todos := Todos} = State) ->
  {reply, maps:find(TodoID, Todos), State};
handle_call({create, Todo}, _From, State) ->
  #{next_todo_id := TodoID, todos := Todos} = State,
  #todo{title = Title, completed = Completed} = Todo,
  NewTodo =
    #persisted_todo{title = Title,
                    id = TodoID,
                    completed = Completed},
  State1 = State#{todos := maps:put(TodoID, NewTodo, Todos), next_todo_id := TodoID + 1},
  {reply, {ok, NewTodo}, State1};
handle_call({update, TodoID, Todo}, _From, #{todos := Todos} = State) ->
  UpdateTodo = fun(PrevTodo) ->
    PrevTodo#persisted_todo{title = Todo#todo.title,
                            completed = Todo#todo.completed}
  end,
  Todos1 =
    case maps:is_key(TodoID, Todos) of
      true ->
        maps:update_with(TodoID, UpdateTodo, Todos);
      false ->
        Todos
    end,
  {reply, maps:find(TodoID, Todos1), State#{todos := Todos1}};
handle_call({delete, TodoID}, _From, #{todos := Todos} = State) ->
  case maps:is_key(TodoID, Todos) of
    true ->
      {reply, ok, State#{todos := maps:remove(TodoID, Todos)}};
    false ->
      {reply, error, State}
  end;
handle_call(_Message, _From, State) ->
  {reply, {error, unknown_message}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.
