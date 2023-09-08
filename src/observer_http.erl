-module(observer_http).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
    worker :: pid(),
    last_input :: binary(),
    from :: tuple()
}).

init(Req, _) ->
    QueryParams = cowboy_req:parse_qs(Req),
	{cowboy_websocket, Req, [QueryParams]}.

websocket_init([QueryParams]) ->
    Worker = proc_lib:spawn_link(erlang, apply, [fun worker_init/1, [get_worker_args(QueryParams)]]),
	{[
        {set_options, #{idle_timeout => infinity}}, 
        {text, <<0, "set_columns|200">>}
    ], #state{worker = Worker, last_input = <<>>}}.

websocket_handle({text, <<0, Msg/binary>>}, State) ->
    gen:reply(State#state.from, Msg),
    {[], State#state{from = undefined}};
websocket_handle({text, Msg}, State) ->
	{[], State#state{last_input = Msg}};
websocket_handle(Data, State) ->
    error_logger:info_msg("got Data ~p~n", [Data]),
	{[], State}.

websocket_info({worker, From, Msg}, State0) ->
    case handle_worker_msg(Msg, From, State0) of
        {reply, Reply, State1} ->
            gen:reply(From, Reply),
            {[], State1};
        {reply, Reply, State1, Commands} ->
            gen:reply(From, Reply),
            {Commands, State1};
        {noreply, State1} ->
            {[], State1};
        {noreply, State1, Commands} ->
            {Commands, State1}
    end;
websocket_info(Info, State) ->
    error_logger:info_msg("got Info ~p~n", [Info]),
	{[], State}.

terminate(Reason, _Req, State) ->
    error_logger:info_msg("Terminating with reason ~p and state ~p~n", [Reason, State]),
    ok.

%% Internal
handle_worker_msg(get_geometry, From, State) ->
    {noreply, State#state{from = From}, [{text, [0, "get_geometry"]}]};
handle_worker_msg(Get, _From, State) when element(1, Get) =:= get_line ->
    Line0 = binary_to_list(State#state.last_input),
    Line1 =
        case Line0 =:= "" of
            true -> "";
            false -> Line0 ++ "\n"
        end,
    {reply, Line1, State#state{last_input = <<>>}};
handle_worker_msg({put_chars, _Encoding, M, F, A}, _From, State) ->
    Text0 = iolist_to_binary(apply(M, F, A)),
    Text1 = binary:replace(Text0, <<"\n">>, <<"\r\n">>, [global]),
    Text2 =
        case Text1 of
            <<Text:(byte_size(Text1)-2)/binary, "\r\n">> ->
                Text;
            _ -> 
                Text1
        end,
    {reply, State#state.last_input, State, [{text, Text2}]};
handle_worker_msg(Msg, _From, State) ->
    error_logger:info_msg("Unhandled message ~p~n", [Msg]),
    {noreply, State}.

get_worker_args(_QueryParams) ->
    [node()].

worker_init([Node | Options]) ->
    ObserverCli = proc_lib:spawn_link(observer_cli, start, [Node, Options]),
    group_leader(self(), ObserverCli),
    [Rows, Columns] = string:split(worker_call(get_geometry), ","),
    worker_loop(ObserverCli, {binary_to_integer(Rows), binary_to_integer(Columns)}).

worker_loop(ObserverCli, Geometry = {Rows, Columns}) ->
    receive
        {io_request, From, ReplyAs, {get_geometry, rows}} ->
            From ! {io_reply, ReplyAs, Rows};
        {io_request, From, ReplyAs, {get_geometry, cols}} ->
            From ! {io_reply, ReplyAs, Columns};
        {io_request, From, ReplyAs, Put = {put_chars, _Encoding, _M, _F, _A}} ->
            From ! {io_reply, ReplyAs, worker_call(Put)};
        {io_request, From, ReplyAs, Get} when element(1, Get) =:= get_line ->
            From ! {io_reply, ReplyAs, worker_call(Get)};
        Msg ->
            error_logger:format("==> ~p~n<== ~p~n", [Msg, worker_call(Msg)])
    end,
    worker_loop(ObserverCli, Geometry).

worker_call(Msg) ->
    Ref = make_ref(),
    hd(get('$ancestors')) ! {worker, {self(), Ref}, Msg},
    receive
        {Ref, Reply} -> Reply
    end.