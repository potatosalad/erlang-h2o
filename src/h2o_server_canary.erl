%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  11 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_server_canary).

-include("h2o_port.hrl").

%% Public API
-export([start_link/2]).

%% Private API
-export([init/2]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Ref, Config) ->
	proc_lib:start_link(?MODULE, init, [Ref, Config]).

%%%===================================================================
%%% Private API
%%%===================================================================

%% @private
init(Ref, Config) ->
	{ok, Server} = h2o_server:open(),
	{ok, Bindings} = h2o_server:setcfg(Server, Config),
	Ports = [Server | [Port || {_, _, _, _, Port} <- Bindings]],
	ok = h2o_kernel:server_canary_register(Ref, Server, Bindings),
	ok = h2o_server:start(Server),
	ok = proc_lib:init_ack({ok, self()}),
	loop(Ports).

%% @private
loop(Ports) ->
	receive
		{h2o_port_closed, Port} ->
			case lists:delete(Port, Ports) of
				Ports ->
					io:format("Got a weird port closed message: ~p~n", [Port]),
					loop(Ports);
				NewPorts ->
					_ = [h2o_port:close(P) || P <- NewPorts],
					exit(closed)
			end;
		Info ->
			io:format("Got a weird canary message: ~p~n", [Info]),
			loop(Ports)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
