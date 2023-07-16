%%%-------------------------------------------------------------------
%% @doc todo_api public API
%% @end
%%%-------------------------------------------------------------------

-module(todo_api_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
  Routes = [
    {"/todos/[:todo_id]", [{todo_id, int}], todos_handler, []},
    {"/[...]", no_match_handler, []}
  ],
  Dispatch = cowboy_router:compile([{'_', Routes}]),
  {ok, _} =
    cowboy:start_clear(http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
  ?LOG_INFO("todo_api started"),
  todo_api_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http_listener).
