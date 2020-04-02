-module(simple_push_auth).

-export([basic_auth/1]).

basic_auth(Req) ->
    %% Check if this user is authenticated
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, <<"test">>, <<"test">>} ->
            {ok, true};
        _ ->
            false
    end.
