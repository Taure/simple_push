%%%-------------------------------------------------------------------
%%% @author daniel <daniel@g501>
%%% @copyright (C) 2020, daniel
%%% @doc
%%%
%%% @end
%%% Created : 31 Mar 2020 by daniel <daniel@g501>
%%%-------------------------------------------------------------------
-module(simple_push_apns).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
-export([send/4]).

-define(SERVER, ?MODULE).

-include("simple_push.hrl").

-record(state, {con = undefined :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

send(Account, DeviceTokens, Message, BundleId) ->
    gen_server:cast(?MODULE, {send, {Account, DeviceTokens, Message, BundleId}}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call(connect, _From, State) ->
    Pid = connect(),
    {reply, Pid, State#state{con = Pid}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast({send, {Account, DeviceToken, Message, BundleId}}, State) ->
    Con = case State#state.con of 
              undefined -> connect();
              _ -> State#state.con
          end,
    Token = new_token(Account),
    NewState = case send_push(Con, Token, DeviceToken, Message, BundleId) of
                   ok -> #state{con = Con};
                   error -> #state{con = undefined}
                end,           
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect() ->
    connect("api.development.push.apple.com", 443).
connect(Host, Port) ->
    {ok, ConnPid} = h2_client:start_link(https, Host, Port, [{mode, binary}]),
    ConnPid.

send_push(Con, JwtToken, DeviceToken, Message, BundleId) ->
    MsgId = uuid:uuid_to_string(uuid:get_v4()),
    ReqHeaders = [{<<":method">>, <<"POST">>},
                {<<":scheme">>, <<"https">>},
                {<<":path">>, <<"/3/device/", DeviceToken/binary>>},
                {<<":authority">>, <<"api.development.push.apple.com">>},
                {<<"authorization">>, JwtToken},
                {<<"apns-priority">>, <<"10">>},
                {<<"apns-topic">>, BundleId},
                {<<"accept">>, <<"*/*">>},
                {<<"accept-encoding">>, <<"gzip, deflate">>},
                {<<"user-agent">>, <<"chatterbox-client/0.0.1">>}
                ],
    ReqBody = json:encode(Message, [maps, binary]),
    {ok, Id} = h2_client:send_request(Con, ReqHeaders, ReqBody),
    receive
        {'EXIT', Reason, Con} -> {error, Reason};
        {'END_STREAM', Id} ->
            {ok, {RespHeaders, RespBody}} = h2_connection:get_response(Con, Id),
            case plist:find(<<":status">>, RespHeaders, <<"600">>) of
                <<"200">> ->
                    ?INFO("Result 200: bundle-id: ~p device-token: ~p, apns-id ~p RespBody: ~p",
                            [BundleId, DeviceToken, MsgId, RespBody]),
                    ok;
                Code  ->
                    ?WARNING("Result ~p: bundle-id: ~p  device-token: ~p apns-id: ~p RespBody: ~p",
                          [Code, BundleId, DeviceToken, MsgId, RespBody]),
                    error
            end
    after 3000 ->
            ?WARNING("Timeout: bundle-id: ~p device-token: ~p apns-id: ~p",
                    [BundleId, DeviceToken, MsgId]),
            error
    end.



new_token(Account) ->
    <<"bearer ", (simple_push_jwt:encode(Account#account.team_id,
                                         Account#account.p8_key,
                                         Account#account.key_id))/binary>>.