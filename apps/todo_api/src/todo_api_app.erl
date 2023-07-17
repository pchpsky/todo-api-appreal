-module(todo_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  todo_db:init(),
  Routes = [
    {"/todos/:todo_id/complete", [{todo_id, int}], todos_handler, []},
    {"/todos/:todo_id", [{todo_id, int}], todos_handler, []},
    {"/todos", todos_handler, []},
    {"/swagger.yaml", cowboy_static, {priv_file, todo_api, "swagger.yaml"}},
    {"/swagger-ui", cowboy_static, {priv_file, todo_api, "swagger-ui/index.html"}},
    {"/swagger-ui/[...]", cowboy_static, {priv_dir, todo_api, "swagger-ui"}},
    {"/[...]", no_match_handler, []}
  ],
  Dispatch = cowboy_router:compile([{'_', Routes}]),
  {ok, _} =
    cowboy:start_clear(http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
  todo_api_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http_listener).
