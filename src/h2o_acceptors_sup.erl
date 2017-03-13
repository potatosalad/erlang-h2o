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
-module(h2o_acceptors_sup).

%% API
-export([start_link/7]).

%% Supervisor callbacks
-export([init/8]).

%% System callbacks
-export([system_code_change/4]).
-export([system_continue/3]).
-export([system_get_state/1]).
-export([system_replace_state/2]).
-export([system_terminate/4]).

%% Records
-record(state, {
	parent  = undefined :: undefined | pid(),
	host    = undefined :: undefined | binary(),
	path    = undefined :: undefined | binary(),
	type    = undefined :: undefined | h2o_filter | h2o_handler | h2o_logger,
	handler = undefined :: undefined | module(),
	opts    = undefined :: undefined | any(),
	suptype = undefined :: undefined | worker | supervisor,
	port    = undefined :: undefined | h2o_port:ref()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Host, Path, Type, Handler, Opts, SupType, Port) ->
	proc_lib:start_link(?MODULE, init, [self(), Host, Path, Type, Handler, Opts, SupType, Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(Parent, Host, Path, Type, Handler, Opts, SupType, Port) ->
	_ = process_flag(trap_exit, true),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	before_loop(#state{parent=Parent, host=Host, path=Path,
		type=Type, handler=Handler, opts=Opts, suptype=SupType,
		port=Port}).

%%%===================================================================
%%% System callbacks
%%%===================================================================

system_code_change(Misc, _Module, _OldVsn, _Extra) ->
	{ok, Misc}.

system_continue(_Parent, _Debug, Misc) ->
	loop(Misc).

system_get_state(Misc) ->
	{ok, Misc}.

system_replace_state(StateFun, Misc) ->
	NState = StateFun(Misc),
	{ok, NState, Misc}.

system_terminate(Reason, _Parent, _Debug, Misc) ->
	terminate(Misc, Reason).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
before_loop(State=#state{host=Host, path=Path, type=Type,
		handler=Handler, opts=Opts, port=Port}) ->
	{ok, Pid} = Type:start_link(Host, Path, Handler, Opts, Port),
	_ = put(Pid, started),
	loop(State).

%% @private
loop(State=#state{parent=Parent, handler=Handler, suptype=SupType, port=Port}) ->
	receive
		% {accepted, Port, Pid} ->
		% 	case put(Pid, accepted) of
		% 		started ->
		% 			before_loop(State);
		% 		accepted ->
		% 			loop(State);
		% 		undefined ->
		% 			before_loop(State)
		% 	end;
		{'EXIT', Parent, Reason} ->
			terminate(State, Reason);
		{'EXIT', Pid, Reason} ->
			case erase(Pid) of
				% accepted ->
				% 	ok = report_error(State, Pid, Reason),
				% 	loop(State);
				started ->
					ok = report_error(State, Pid, Reason),
					before_loop(State);
				undefined ->
					loop(State)
			end;
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
		{'$gen_call', {To, Tag}, which_children} ->
			Children = [{Handler, Pid, SupType, [Handler]}
				|| {Pid, Mode} <- get(),
				Mode == active orelse Mode == removed],
			To ! {Tag, Children},
			loop(State);
		{'$gen_call', {To, Tag}, count_children} ->
			NbChildren = length([1 || {_, Mode} <- get(),
				Mode == active orelse Mode == removed]),
			Counts = case SupType of
				worker -> [{supervisors, 0}, {workers, NbChildren}];
				supervisor -> [{supervisors, NbChildren}, {workers, 0}]
			end,
			Counts2 = [{specs, 1}, {active, NbChildren} | Counts],
			To ! {Tag, Counts2},
			loop(State);
		{'$gen_call', {To, Tag}, _Request} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State);
		Msg ->
			error_logger:error_msg(
				"h2o acceptors supervisor ~p received unexpected message ~p~n",
				[Port, Msg])
	end.

terminate(_State, Reason) ->
	exit(Reason).

%% @private
report_error(_State, _Pid, normal) ->
	ok;
report_error(_State, _Pid, shutdown) ->
	ok;
report_error(_State, _Pid, {shutdown, _}) ->
	ok;
report_error(#state{type=Type, handler=Handler, port=Port}, Pid, Reason) ->
	error_logger:error_msg(
		"h2o binding supervisor ~p had acceptor process started with "
		"~p:start_link/5 at ~p (with ~p) exit with reason: ~999999p~n",
		[Port, Type, Pid, Handler, Reason]).
