%%%-------------------------------------------------------------------
%% @doc todo_api public API
%% @end
%%%-------------------------------------------------------------------

-module(todo_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    todo_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
