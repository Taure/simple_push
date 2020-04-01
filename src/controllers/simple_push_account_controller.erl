-module(simple_push_account_controller).
-export([
         apns/1
        ]).

-include("simple_push.hrl").

apns(#{method := <<"POST">>} = Req) ->
    {ok, Data, _Req0} = cowboy_req:read_body(Req),
    JSON = json:decode(Data, [maps]),
    Account = #account{id = maps:get(id, JSON),
                       team_id = maps:get(team_id, JSON),
                       key_id = maps:get(key_id, JSON),
                       p8_key = maps:get(p8_key, JSON)},
    {ok, Account1} = simple_push_db:create_account(Account),
    {json, #{id => Account1#account.id}}.
