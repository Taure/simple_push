-module(simple_push_send_controller).

-export([send/1]).

-include("simple_push.hrl").

send(#{bindings := #{push := PushId}} = Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    #{<<"payload">> := Message,
      <<"push_tokens">> := DeviceTokens,
      <<"bundle_id">> := BundleId} = json:decode(Body,[maps, binary]),
    case simple_push_db:fetch_account(#account{id = PushId}) of
        {ok, Account} ->
                simple_push_apns:send(Account, DeviceTokens, Message, BundleId),
                {status, 200};
        {error, not_found} ->
                {status, 404}
    end.