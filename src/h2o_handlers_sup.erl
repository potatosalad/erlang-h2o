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
-module(h2o_handlers_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Ref) ->
	supervisor:start_link(?MODULE, {Ref}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init({Ref}) ->
	h2o_stat = ets:new(h2o_stat, [ordered_set, public, named_table]),
	Bindings = h2o_kernel:server_canary_bindings(Ref),
	% io:format("Bindings = ~p~n", [Bindings]),
	ChildSpecs = child_specs(Bindings, []),
	Restart = {one_for_one, 1, 5},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
child_specs([{Host, Path, h2o_handler, {Handler, Opts}, Port} | Bindings], ChildSpecs) ->
	ChildSpec = {{h2o_handler_sup, Port},
		{h2o_handler_sup, start_link, [Host, Path, {Handler, Opts}, Port]},
		permanent, infinity, supervisor, [h2o_handler_sup]
	},
	child_specs(Bindings, [ChildSpec | ChildSpecs]);
child_specs([_ | Bindings], ChildSpecs) ->
	child_specs(Bindings, ChildSpecs);
child_specs([], ChildSpecs) ->
	lists:reverse(ChildSpecs).
