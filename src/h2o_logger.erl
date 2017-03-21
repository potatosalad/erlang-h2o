%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  15 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_logger).

-callback log_access(Req :: h2o_port:ref(), Host :: binary(), Path :: binary(), Opts :: any()) -> ok.

%% Public API
-export([start_link/5]).
-export([flush/1]).
-export([read/1]).

%% Private API
-export([init/6]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Host, Path, Handler, Opts, Port) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [self(), Host, Path, Handler, Opts, Port]),
	{ok, Pid}.

flush(Port) ->
	receive
		{h2o_port_data, Port, Data} ->
			ok = io:format("~s", [Data]),
			flush(Port)
	after
		0 ->
			ok
	end.

read(Port) ->
	ok = h2o_nif:logger_read_start(Port),
	receive
		{h2o_port_data, Port, ready_input} ->
			h2o_nif:logger_read(Port)
	end.

%%%===================================================================
%%% Private API
%%%===================================================================

init(_Parent, Host, Path, Handler, Opts, Port) ->
	case h2o_port:accept(Port) of
		{ok, Socket} ->
			ok = Handler:execute(Socket, Host, Path, Opts),
			exit(normal)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
