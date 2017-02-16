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
-module(h2o).

-include("h2o.hrl").

%% API
-export([start/0]).

% -define(MAYBE_START_H2O(F), try
% 	F
% catch
% 	_:_ ->
% 		_ = h2o:start(),
% 		F
% end).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	application:ensure_all_started(?MODULE).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
