-record(account, {
                  id,
                  team_id,
                  key_id,
                  p8_key
                 }).
-type account() :: #account{}.

-ifndef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(), ).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.

-define(DEBUG(M), simple_push_log:log(debug, M)).
-define(DEBUG(M, Meta), simple_push_log:log(debug, M, Meta)).
-define(INFO(M), simple_push_log:log(info, M)).
-define(INFO(M,Meta), simple_push_log:log(info, M, Meta)).
-define(WARNING(M), simple_push_log:log(warning, M)).
-define(WARNING(M,Meta), simple_push_log:log(warning, M, Meta)).
-define(ERROR(M), simple_push_log:log(error, M)).
-define(ERROR(M,Meta), simple_push_log:log(error, M, Meta)).
-define(UNEXPECTED(Type, Content),
        ?WARNING("Unexpected ~p: ~p", [Type, Content])).