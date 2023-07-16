-module(helpers).

-export([read_json_body/1, reply/3, get_route/1, const/1]).

read_json_body(Req) ->
  {ok, [{Body, true}], Req1} = cowboy_req:read_urlencoded_body(Req),
  case jsx:is_json(Body) of
    true ->
      {ok, jsx:decode(Body), Req1};
    false ->
      {error, reply(400, #{error => "Bad request"}, Req)}
  end.

reply(Status, Body = #{}, Req) ->
  cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Body), Req).

get_route(Req) ->
  {cowboy_req:method(Req), cowboy_req:path(Req)}.

const(Val) ->
  fun (_) -> Val end.
