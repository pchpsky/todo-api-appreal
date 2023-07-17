-module(todos_handler).

-include("include/todo.hrl").

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
         list_todos/2, show_todo/2, create_todo/2, update_todo/2, complete_todo/2,
         delete_resource/2]).

-import(helpers, [reply/3, get_route/1, read_json_body/1, const/1]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {_, Route} = get_route(Req),
  case Route of
    [<<"todos">>] ->
      {[<<"GET">>, <<"POST">>], Req, State};
    [<<"todos">>, <<_/binary>>] ->
      {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State};
    [<<"todos">>, <<_/binary>>, <<"complete">>] ->
      {[<<"POST">>], Req, State}
  end.

content_types_provided(Req, State) ->
  case get_route(Req) of
    {<<"GET">>, [<<"todos">>]} ->
      {[{{<<"application">>, <<"json">>, []}, list_todos}], Req, State};
    {<<"GET">>, [<<"todos">>, <<_/binary>>]} ->
      {[{{<<"application">>, <<"json">>, []}, show_todo}], Req, State};
    _ ->
      {[{{<<"application">>, <<"json">>, []}, no_call}], Req, State}
  end.

content_types_accepted(Req, State) ->
  logger:info("content_types_accepted Route: ~p", [get_route(Req)]),
  case get_route(Req) of
    {<<"POST">>, [<<"todos">>]} ->
      {[{{<<"application">>, <<"json">>, []}, create_todo}], Req, State};
    {<<"POST">>, [<<"todos">>, <<_/binary>>, <<"complete">>]} ->
      {[{{<<"application">>, <<"json">>, []}, complete_todo}], Req, State};
    {<<"PUT">>, [<<"todos">>, <<_/binary>>]} ->
      {[{{<<"application">>, <<"json">>, []}, update_todo}], Req, State}
  end.

list_todos(Req, State) ->
  Todos = todo_db:all(),
  Todos1 = lists:map(fun todo_to_map/1, Todos),
  Resp = jsx:encode(#{<<"todos">> => Todos1}),
  {Resp, Req, State}.

show_todo(Req, State) ->
  TodoId = cowboy_req:binding(todo_id, Req),
  case todo_db:find(TodoId) of
    {ok, Todo} ->
      Resp = jsx:encode(todo_to_map(Todo)),
      {Resp, Req, State};
    {error, not_found} ->
      {false, reply(404, #{message => <<"Not found">>}, Req), State}
  end.

create_todo(Req, State) ->
  Req2 =
    case read_json_body(Req) of
      {ok, TodoProps, Req1} ->
        case todo_db:insert(todo_from_map(TodoProps)) of
          {ok, Todo} ->
            reply(200, todo_to_map(Todo), Req1)
        end;
      {error, Req1} ->
        Req1
    end,
  {stop, Req2, State}.

update_todo(Req, State) ->
  Req2 =
    case read_json_body(Req) of
      {ok, TodoProps, Req1} ->
        TodoId = cowboy_req:binding(todo_id, Req),
        case todo_db:update(TodoId, const(todo_from_map(TodoProps))) of
          {ok, Todo} ->
            reply(200, todo_to_map(Todo), Req1);
          {error, not_found} ->
            reply(404, #{message => <<"Not found">>}, Req1)
        end;
      {error, Req1} ->
        Req1
    end,
  {stop, Req2, State}.

complete_todo(Req, State) ->
  TodoId = cowboy_req:binding(todo_id, Req),
  Req1 =
    case todo_db:update(TodoId, fun(Todo) -> Todo#todo{completed = true} end) of
      {ok, Todo} ->
        reply(200, todo_to_map(Todo), Req);
      {error, not_found} ->
        reply(404, #{message => <<"Not found">>}, Req)
    end,
  {stop, Req1, State}.

delete_resource(Req, State) ->
  TodoId = cowboy_req:binding(todo_id, Req),
  case todo_db:delete(TodoId) of
    ok ->
      {true, Req, State};
    {error, not_found} ->
      {false, reply(404, #{message => <<"Not found">>}, Req), State}
  end.

todo_from_map(Map) ->
  #{<<"title">> := Title} = Map,
  Completed = maps:get(<<"completed">>, Map, false),
  #todo_props{title = Title, completed = Completed}.

todo_to_map(Todo = #todo{}) ->
  #todo{id = TodoID,
        title = Title,
        completed = Completed} =
    Todo,
  #{<<"id">> => TodoID,
    <<"title">> => Title,
    <<"completed">> => Completed}.
