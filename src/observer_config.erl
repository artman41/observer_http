-module(observer_config).
-export([
    get_standalone/0,
    get_web_route/0,
    get_web_port/0,
    get_num_acceptors/0
]).

get_standalone() ->
    get_env(standalone, true).

get_web_route() ->
    get_env(web_route, "/observer").

get_web_port() ->
    get_env(web_port, 8080).

get_num_acceptors() ->
    get_env(num_acceptors, 100).

%%

get_env(Key) ->
    application:get_env(observer_http, Key).

get_env(Key, Default) ->
    case get_env(Key) of
        undefined ->
            Default;
        {ok, V} ->
            V
    end.