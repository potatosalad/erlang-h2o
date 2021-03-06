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
-export([open/0]).
% -export([open/1]).
% % -export([open/1]).
% -export([close/1]).
% % -export([controlling_process/2]).
% % -export([to_id/1]).
-export([getcfg/1]).
-export([setcfg/2]).
-export([start/1]).

% % %% Types
% % -type id() :: integer().
% % -type ref() :: {id(), binary() | reference()}.
% % -type obj() :: #h2o_port{}.
% % -export_type([id/0]).
% % -export_type([ref/0]).
% % -export_type([obj/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

open() ->
	Port = h2o_nif:server_open(),
	true = h2o_kernel:port_connect(Port),
	{ok, Port}.

% open(Config) ->
% 	case open() of
% 		{ok, Port} ->
% 			case setcfg(Port, Config) of
% 				{ok, Bindings} ->
% 					{ok, Port, Bindings};
% 				{error, _} = Error ->
% 					Error
% 			end;
% 		{error, _} = Error ->
% 			Error
% 	end.

% close(Port) ->
% 	h2o_port:close(Port).

% % controlling_process(Port, NewOwner) ->
% % 	h2o_port:controlling_process(Port, NewOwner).

% % to_id(Port) ->
% % 	h2o_port:to_id(Port).

getcfg(Port) ->
	h2o_nif:server_getcfg(Port).

setcfg(Port, Config0) ->
	{Config1, Bindings0} = h2o_config:encode(Config0),
	io:format("config:~n~s~n", [Config1]),
	try h2o_nif:server_setcfg(Port, Config1) of
		ok ->
			Bindings1 = receive_bindings(Bindings0, []),
			{ok, Bindings1}
	catch
		error:Reason ->
			{error, Reason}
	end.

start(Port) ->
	h2o_nif:server_start(Port).

% % setcfg(Port, Config) ->
% % 	h2o_nif:server_setcfg(Port, h2o_yaml:encode(Config)).

% %%%-------------------------------------------------------------------
% %%% Internal functions
% %%%-------------------------------------------------------------------

%% @private
receive_bindings([{Module, Ref, Path, Opts} | Bindings], Acc) ->
	receive
		{Ref, Port} ->
			receive_bindings(Bindings, [{Module, Port, Path, Opts} | Acc])
	after
		0 ->
			erlang:error({badarg, [Module, Ref, Path, Opts]})
	end;
receive_bindings([], Acc) ->
	lists:reverse(Acc).
