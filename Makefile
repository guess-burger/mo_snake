PROJECT = mo_snake

DEPS = cowboy jsx meck

dep_meck = git https://github.com/eproxus/meck.git 0.8

SHELL_OPTS = -pa ../mo_snake -eval 'application:ensure_all_started(mo_snake)'


elm:
	cd priv/elm && elm-make Snake.elm --output=snake.js


include erlang.mk
