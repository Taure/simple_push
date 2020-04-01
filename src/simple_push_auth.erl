-module(simple_push_auth).


basic_auth(Req) ->
    %% Check if this user is authenticated
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, <<"test">>, <<"test">>} ->
            true;
        _ ->
            false
    end.
