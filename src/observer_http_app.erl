-module(observer_http_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	case observer_config:get_standalone() of
		true ->
			observer_http_sup:start_link();
		false ->
			{ok, self()}
	end.

stop(_State) ->
	observer_config:get_standalone() andalso observer_http_sup:stop().
