%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  09 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_gc).

%% Public API
-export([start_link/1]).

%% Private API
-export([init/2]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link([h2o_port:ref()])
	-> {ok, pid()} | ignore | {error, term()}.
start_link(Ports) ->
	proc_lib:start_link(?MODULE, init, [Ports, self()]).

%%%===================================================================
%%% Private API
%%%===================================================================

-spec init([h2o_port:ref()], pid())
	-> no_return().
init(Ports, Parent) ->
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	% io:format("GC start~n"),
	true = h2o_nif:port_gc(),
	receive
		h2o_gc_stop ->
			Parent ! {h2o_gc_stop, self()},
			exit(normal)
	after
		10 ->
			loop(Ports, Parent)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
loop(Ports, Parent) ->
	receive
		h2o_gc_stop ->
			Parent ! {h2o_gc_stop, self()},
			exit(normal)
	after
		0 ->
			cleanup(Ports, 100, Parent)
	end.

%% @private
cleanup(Ports, 0, Parent) ->
	loop(Ports, Parent);
cleanup([Port | Ports], N, Parent) ->
	% io:format("cleanup ~p~n", [Port]),
	case h2o_nif:port_is_alive(Port) of
		true ->
			cleanup(Ports, N - 1, Parent);
		false ->
			Parent ! {h2o_gc_port, self(), Port},
			cleanup(Ports, N - 1, Parent)
	end;
cleanup([], _N, Parent) ->
	Parent ! {h2o_gc_stop, self()},
	exit(normal).
