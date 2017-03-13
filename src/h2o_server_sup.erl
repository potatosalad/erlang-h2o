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
-module(h2o_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Ref, Config) ->
	supervisor:start_link(?MODULE, {Ref, Config}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init({Ref, Config}) ->
	ChildSpecs = [
		{h2o_server_canary,
			{h2o_server_canary, start_link, [Ref, Config]},
			permanent, infinity, worker, [h2o_server_canary]},
		{h2o_bindings_sup,
			{h2o_bindings_sup, start_link, [Ref]},
			permanent, infinity, supervisor, [h2o_bindings_sup]}
	],
	Restart = {rest_for_one, 1, 5},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

% %% @private
% child_specs(Bindings) ->
% 	{Filters, Handlers, Loggers} = partition_bindings(Bindings, [], [], []),
% 	ChildSpecs0 = [],
% 	ChildSpecs1 = maybe_start_filters(Filters, ChildSpecs0),
% 	ChildSpecs2 = maybe_start_handlers(Handlers, ChildSpecs1),
% 	ChildSpecs3 = maybe_start_loggers(Loggers, ChildSpecs2),
% 	lists:reverse(ChildSpecs3).

% %% @private
% maybe_start_filters([], ChildSpecs) ->
% 	ChildSpecs;
% maybe_start_filters(Filters, ChildSpecs) ->
% 	ChildSpec = {h2o_filters_sup,
% 		{h2o_filters_sup, start_link, [Filters]},
% 		permanent, infinity, supervisor, [h2o_filters_sup]
% 	},
% 	[ChildSpec | ChildSpecs].

% %% @private
% maybe_start_handlers([], ChildSpecs) ->
% 	ChildSpecs;
% maybe_start_handlers(Handlers, ChildSpecs) ->
% 	ChildSpec = {h2o_handlers_sup,
% 		{h2o_handlers_sup, start_link, [Handlers]},
% 		permanent, infinity, supervisor, [h2o_handlers_sup]
% 	},
% 	[ChildSpec | ChildSpecs].

% %% @private
% maybe_start_loggers([], ChildSpecs) ->
% 	ChildSpecs;
% maybe_start_loggers(Loggers, ChildSpecs) ->
% 	ChildSpec = {h2o_loggers_sup,
% 		{h2o_loggers_sup, start_link, [Loggers]},
% 		permanent, infinity, supervisor, [h2o_loggers_sup]
% 	},
% 	[ChildSpec | ChildSpecs].

% %% @private
% partition_bindings([H={_Host, _Path, handler, {_Handler, _Opts}, _Port} | Bindings], Filters, Handlers, Loggers) ->
% 	partition_bindings(Bindings, Filters, [H | Handlers], Loggers);
% partition_bindings([], Filters, Handlers, Loggers) ->
% 	{lists:reverse(Filters), lists:reverse(Handlers), lists:reverse(Loggers)}.
