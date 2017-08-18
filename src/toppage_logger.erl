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
-module(toppage_logger).

-include("h2o_req.hrl").

%% API
-export([log_access/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

log_access(_Events, []) ->
	% io:format("Events: ~p~n", [length(_Events)]),
	% _ = [io:format("Event: ~p~n", [Event]) || Event <- _Events],
	ok.
