%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  30 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_logger).

-include("h2o_req.hrl").

%% Public API
-export([start_link/3]).
-export([pretty_print/1]).

%% Private API
-export([init/4]).

%% Records
-record(state, {
	parent = undefined :: undefined | pid(),
	port   = undefined :: undefined | reference(),
	path   = undefined :: undefined | [binary()],
	opts   = undefined :: undefined | any()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Port, Path, Opts) ->
	proc_lib:start_link(?MODULE, init, [self(), Port, Path, Opts]).

%%%===================================================================
%%% Private API
%%%===================================================================

%% @private
init(Parent, Port, Path, Opts) ->
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	ok = receive
		{shoot, Parent, Port} ->
			ok
	end,
	ok = h2o_nif:logger_read_start(Port),
	State = #state{parent=Parent, port=Port, path=Path, opts=Opts},
	loop(State).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
loop(State=#state{port=Port}) ->
	receive
		{h2o_port_data, Port, ready_input} ->
			dispatch(h2o_nif:logger_read(Port), State);
		Info ->
			error_logger:error_msg(
				"~p received unexpected message ~p~n",
				[?MODULE, Info]),
			loop(State)
	end.

pretty_print(Record) ->
	io_lib_pretty:print(Record, fun pretty_print/2).

pretty_print(h2o_req, N) ->
	N = record_info(size, h2o_req) - 1,
	record_info(fields, h2o_req);
pretty_print(h2o_res, N) ->
	N = record_info(size, h2o_res) - 1,
	record_info(fields, h2o_res);
pretty_print(_, _) ->
	[].

%% @private
dispatch(Events, State=#state{opts={Filter, Opts, _, _}}) ->
	ok = Filter:log_access(Events, Opts),
	loop(State).
