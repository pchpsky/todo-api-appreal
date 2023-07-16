-module(no_match_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {ok, helpers:reply(404, #{message => <<"Not found">>}, Req), State}.
