-module(mo_snake_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		{lobby, {lobby, start_link, []}, permanent, 5000, worker, [lobby]}
	],

	% FIXME maybe have a better supervision hierarchy
	% Maybe, this sup has a lobby gen_server and a match sup

	{ok, {{one_for_one, 1, 5}, Procs}}.
