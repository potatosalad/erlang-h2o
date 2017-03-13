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
-module(h2o_server_canary).

-behaviour(gen_server).

-include("h2o_port.hrl").

%% Public API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Ref, Config) ->
	proc_lib:start_link(?MODULE, init, [{Ref, Config}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Ref, Config}) ->
	{ok, Server} = h2o_server:open(),
	{ok, Bindings} = h2o_server:setcfg(Server, Config),
	State = [Server | [Port || {_Host, _Path, _Type, _Handler, _Opts, _SupType, _NbAcceptors, Port} <- Bindings]],
	ok = h2o_kernel:server_canary_register(Ref, Server, Bindings),
	ok = h2o_server:start(Server),
	ok = proc_lib:init_ack({ok, self()}),
	gen_server:enter_loop(?MODULE, [], State).

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({h2o_port_closed, Port}, State) ->
	case lists:delete(Port, State) of
		State ->
			{noreply, State};
		NewState ->
			_ = [h2o_port:close(P) || P <- NewState],
			{stop, closed, State}
	end;
handle_info(Info, State) ->
	error_logger:error_msg(
		"h2o server canary ~p received unexpected message ~p~n",
		[self(), Info]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
