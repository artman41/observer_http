-module(observer_http_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).
-export([cowboy_dispatch/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	gen_server:stop(?MODULE).

init([]) ->
	Procs = [
		cowboy_childspec()
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.

cowboy_dispatch() ->
	Route = observer_config:get_web_route(),
	cowboy_router:compile([
		{'_', [
			{filename:join(Route, "/ws"), observer_http, []},
			{Route, cowboy_static, {priv_file, observer_http, "index.html"}}
		]}
	]).

cowboy_childspec() ->
	Ref = websrv,
	TransOpts = #{
		socket_opts => [{port, observer_config:get_web_port()}], 
		num_acceptors => observer_config:get_num_acceptors()
	},
	ProtoOpts = #{
		env => #{dispatch => cowboy_dispatch()}
	},
	ranch:child_spec(Ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).