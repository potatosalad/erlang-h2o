%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  13 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_bindings_sup).

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
	Bindings = h2o_kernel:server_canary_bindings(Ref),
	ChildSpecs = child_specs(Bindings, []),
	Restart = {one_for_one, 1, 5},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
child_specs([{Host, Path, Type, Handler, Opts, SupType, NbAcceptors, Port} | Bindings], ChildSpecs) ->
	ChildSpec = {{h2o_binding_sup, Port},
		{h2o_binding_sup, start_link, [Host, Path, Type, Handler, Opts, SupType, NbAcceptors, Port]},
		permanent, infinity, supervisor, [h2o_binding_sup]
	},
	child_specs(Bindings, [ChildSpec | ChildSpecs]);
child_specs([], ChildSpecs) ->
	lists:reverse(ChildSpecs).
