%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  27 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_filter).

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
	ok = h2o_nif:filter_read_start(Port),
	State = #state{parent=Parent, port=Port, path=Path, opts=Opts},
	loop(State).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
loop(State=#state{port=Port}) ->
	receive
		{h2o_port_data, Port, ready_input} ->
			dispatch(h2o_nif:filter_read(Port), State);
		{h2o_port_data, EventPort, ready_input} ->
			% io:format("ready_input~n"),
			case get(EventPort) of
				undefined ->
					loop(State);
				{Filter, FilterState0} ->
					{ok, FilterState1} = Filter:on_ready_input(FilterState0),
					_ = put(EventPort, {Filter, FilterState1}),
					loop(State)
			end;
		{h2o_port_data, EventPort, final_input} ->
			% io:format("final_input~n"),
			case erase(EventPort) of
				undefined ->
					loop(State);
				{Filter, FilterState} ->
					ok = Filter:on_final_input(FilterState),
					loop(State)
			end;
		{h2o_port_data, EventPort, Input} ->
			% io:format("input: ~p~n", [Input]),
			case get(EventPort) of
				undefined ->
					loop(State);
				{Filter, FilterState0} ->
					{ok, FilterState1} = Filter:on_input(Input, FilterState0),
					_ = put(EventPort, {Filter, FilterState1}),
					loop(State)
			end;
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
dispatch([Event=#h2o_req{event=EventPort} | Events], State=#state{opts={Filter, Opts}}) ->
	% io:format("~s~n", [pretty_print(Event)]),
	{ok, FilterState} = Filter:on_setup_ostream(Event, Opts),
	undefined = put(EventPort, {Filter, FilterState}),
	dispatch(Events, State);
% dispatch([Event | Events], State=#state{opts={Filter, Opts}}) ->
% 	io:format("~s~n", [pretty_print(Event)]),
% 	{ok, FilterState} = Filter:on_setup_ostream(Event, Opts),
% 	undefined = put(Event, {Filter, FilterState}),
% 	dispatch(Events, State);
dispatch([], State) ->
	loop(State).
