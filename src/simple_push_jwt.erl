%%%-------------------------------------------------------------------
%%% @doc
%%%   
%%% @end
%%%
%% @copyright (C) 2020, Zaark AB <dev@zaark.com>
%%%-------------------------------------------------------------------
-module(simple_push_jwt).
-copyright('Zaark AB <dev@zaark.com>').

%% API
-export([encode/3]).

%% Includes
-include_lib("public_key/include/public_key.hrl").


%% ===================================================================
%% API
%% ===================================================================

encode(ISS, Key, KeyId) ->
    Header = json:encode({[{alg, <<"ES256">>}, {typ, <<"JWT">>}, {kid, KeyId}]},
                         [binary]),
    Time = erlang:system_time(seconds),
    Content = json:encode({[{iss, ISS}, {iat, Time}]}, [binary]),
    Data = <<(do_encode(Header))/binary, $., (do_encode(Content))/binary>>,
    ECPrivateKeyPem = case public_key:pem_decode(Key) of
                          [Pem] -> Pem;
                          [_, Pem] -> Pem
                     end,
    ECPrivateKey = public_key:pem_entry_decode(ECPrivateKeyPem),
    Sign = do_encode(public_key:sign(Data, sha256, ECPrivateKey)),
    <<Data/binary, $., Sign/binary>>.

%% ===================================================================
%% Internal functions.
%% ===================================================================

do_encode(X) -> strip_url(base64:encode(X), <<>>).

strip_url(<<>>, Acc) -> Acc;
strip_url(<<$=>>, Acc) -> Acc;
strip_url(<<$=, $=>>, Acc) -> Acc;
strip_url(<<$/, T/binary>>, Acc) -> strip_url(T, <<Acc/binary, $_>>);
strip_url(<<$+, T/binary>>, Acc) -> strip_url(T, <<Acc/binary, $->>);
strip_url(<<H, T/binary>>, Acc) -> strip_url(T, <<Acc/binary, H>>).
