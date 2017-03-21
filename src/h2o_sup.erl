%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  16 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	h2o_kernel = ets:new(h2o_kernel, [ordered_set, public, named_table]),
	ChildSpecs = [
		?CHILD(h2o_kernel, worker)%,
		% ?CHILD(h2o_monitor, worker)
	],
	Restart = {one_for_one, 10, 10},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
