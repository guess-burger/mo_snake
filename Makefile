PROJECT = mo_snake

DEPS = cowboy jsx sync

include erlang.mk

SHELL_OPTS = -pa ../mo_snake -eval 'application:ensure_all_started(sync)' -eval 'application:ensure_all_started(mo_snake)'


elm:
	cd priv/elm && elm make Snake.elm --output=snake.js
