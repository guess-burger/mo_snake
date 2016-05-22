-module(mo_snake_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [
      {"/", cowboy_static, {file, "priv/static/index.html"}},
      {"/elm/snake.js", cowboy_static, {file, "priv/elm/snake.js"}},
      {"/resources/[...]", cowboy_static, {priv_dir, mo_snake, "static"}},
      {"/echo", websocket_handler, []}
    ]}
  ]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
  cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ).

stop(_State) ->
	ok.
