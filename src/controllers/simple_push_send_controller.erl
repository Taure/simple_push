-module(simple_push_send_controller).

-export([send/1]).

send(#{bindings := #{push := PushId}} = Req) ->
    {ok, Body, Req} = cowboy_req:read_body(Req),
    #{<<"payload">> := Message,
      <<"device_tokens">> := DeviceTokens} = json:decode(Body,[maps, binary]),
    Account = simple_push_db:fetch_account(PushId),
    simple_push_apns:send(Account, DeviceTokens, Message),
    200.