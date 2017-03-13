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
-module(h2o_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Host, Path, {Handler, Opts}, Port) ->
	supervisor:start_link(?MODULE, {Host, Path, {Handler, Opts}, Port}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init({Host, Path, {Handler, Opts}, Port}) ->
	ChildSpecs = [
		{{handler, self(), N},
			{h2o_handler, start_link, [Host, Path, {Handler, Opts}, Port]},
			permanent, brutal_kill, worker, []}
		|| N <- lists:seq(1, 1)
	],
	Restart = {one_for_one, 1, 5},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
