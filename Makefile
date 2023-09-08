PROJECT = observer_http
PROJECT_DESCRIPTION = Web wrapper for observer_cli
PROJECT_VERSION = 0.1.0

define PROJECT_ENV
[
	{standalone, true},
	{web_route, "/observer"},
	{web_port, 8080},
	{num_acceptors, 100}
]
endef

BUILD_DEPS += relx

DEPS += observer_cli 
DEPS += cowboy
DEPS += ranch

LOCAL_DEPS += inets

dep_observer_cli = git https://github.com/zhongwencool/observer_cli.git
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.10.0
dep_ranch = git https://github.com/ninenines/ranch.git

SHELL_OPTS += -eval 'application:ensure_all_started(observer_http).'

include erlang.mk
