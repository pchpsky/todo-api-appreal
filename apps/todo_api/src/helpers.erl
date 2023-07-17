-module(helpers).

-export([parse_json_with_schema/2, reply/3, get_route/1, const/1]).

parse_json_with_schema(Schema, Req) ->
  {ok, [{Body, true}], Req1} = cowboy_req:read_urlencoded_body(Req),
  case jsx:is_json(Body) of
    true ->
      case jesse:validate_with_schema(Schema, jsx:decode(Body)) of
        {ok, Props} ->
          {ok, Props, Req1};
        {error, Reasons} ->
          MakeError = fun (Reason) ->
            maps:with([error, data, path], maps:from_list(jesse_error:reason_to_jsx(Reason)))
          end,
          Errors = lists:map(MakeError, Reasons),
          {error, {422, #{message => <<"Unprocessable entity">>, errors => Errors}}, Req1}
      end;
    false ->
      {error, {400, #{message => <<"Bad request">>}}, Req1}
    end.

reply(Status, Body = #{}, Req) ->
  cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Body), Req).

get_route(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  {cowboy_req:method(Req), Path}.

const(Val) ->
  fun (_) -> Val end.
