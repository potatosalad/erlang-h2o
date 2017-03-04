%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  03 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_server).

-include("h2o_port.hrl").

%% Public API
-export([open/1]).
% -export([open/1]).
-export([close/1]).
% -export([controlling_process/2]).
% -export([to_id/1]).
-export([getcfg/1]).
% -export([setcfg/2]).
-export([start/1]).

% %% Types
% -type id() :: integer().
% -type ref() :: {id(), binary() | reference()}.
% -type obj() :: #h2o_port{}.
% -export_type([id/0]).
% -export_type([ref/0]).
% -export_type([obj/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

open(Config) ->
	Config2 = h2o_yaml:encode(Config),
	Port = #h2o_port{id=ID} = h2o_nif:server_open(),
	true = h2o_kernel:port_connect(ID),
	try h2o_nif:server_setcfg(Port, Config2) of
		ok ->
			{ok, Port}
	catch
		error:Reason ->
			{error, Reason}
	end.

close(Port) ->
	h2o_port:close(Port).

% controlling_process(Port, NewOwner) ->
% 	h2o_port:controlling_process(Port, NewOwner).

% to_id(Port) ->
% 	h2o_port:to_id(Port).

getcfg(Port) ->
	h2o_nif:server_getcfg(Port).

start(Port) ->
	h2o_nif:server_start(Port).

% setcfg(Port, Config) ->
% 	h2o_nif:server_setcfg(Port, h2o_yaml:encode(Config)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

% %% @private
% sync_input(P, Owner, Flag) ->
% 	sync_input(P, to_id(P), Owner, Flag).

% %% @private
% sync_input(P, ID, Owner, Flag) ->
% 	receive
% 		{h2o_port_data, ID, Data} ->
% 			Owner ! {h2o_port_data, ID, Data},
% 			sync_input(P, ID, Owner, Flag);
% 		{h2o_port_closed, ID} ->
% 			Owner ! {h2o_port_closed, ID},
% 			sync_input(P, ID, Owner, true)
% 	after
% 		0 ->
% 			Flag
% 	end.
