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
-module(h2o_handler).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/4]).
-export([init_handler/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Host, Path, {Handler, Opts}, Port) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [Host, Path, {Handler, Opts}, Port]),
	{ok, Pid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(Host, Path, {Handler, Opts}, Port) ->
	_ = process_flag(trap_exit, true),
	accept_loop(Host, Path, {Handler, Opts}, Port).
	% before_loop(Host, Path, {Handler, Opts}, Port).

init_handler(Host, Path, {Handler, Opts}, Port) ->
	% io:format("Accepting ~p for ~p~n", [self(), Port]),
	{ok, Socket} = h2o_port:accept(Port),
	% io:format("Accepted  ~p for ~p~n", [self(), Port]),
	Handler:execute(Socket, Host, Path, Opts).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

accept_loop(Host, Path, {Handler, Opts}, Port) ->
	% {ok, Socket} = h2o_port:accept(Port),
	{Time, {ok, Socket}} = timer:tc(h2o_port, accept, [Port]),
	_ = case ets:lookup(h2o_stat, accept) of
		[] ->
			ets:insert(h2o_stat, {accept, 0});
		[{accept, 0}] ->
			ets:insert(h2o_stat, {accept, Time});
		[{accept, OldTime}] ->
			% ets:insert(h2o_stat, {accept, ((OldTime + Time) div 2) + ((OldTime + Time) rem 2)})
			ets:insert(h2o_stat, {accept, (OldTime + Time) / 2})
	end,
	% io:format("~p~n", [Time]),
	{HTime, ok} = timer:tc(Handler, execute, [Socket, Host, Path, Opts]),
	_ = case ets:lookup(h2o_stat, execute) of
		[] ->
			ets:insert(h2o_stat, {execute, HTime});
		[{execute, OldHTime}] ->
			% ets:insert(h2o_stat, {execute, ((OldHTime + HTime) div 2) + ((OldHTime + HTime) rem 2)})
			ets:insert(h2o_stat, {execute, (OldHTime + HTime) / 2})
	end,
	% ok = Handler:execute(Socket, Host, Path, Opts),
	accept_loop(Host, Path, {Handler, Opts}, Port).

% before_loop(Host, Path, {Handler, Opts}, Port) ->
% 	Pid = proc_lib:spawn_link(?MODULE, init_handler, [Host, Path, {Handler, Opts}, Port]),
% 	loop(Host, Path, {Handler, Opts}, Port, Pid).

% loop(Host, Path, {Handler, Opts}, Port, Pid) ->
% 	receive
% 		{'EXIT', Pid, _Reason} ->
% 			% io:format("EXIT: ~p~n", [Reason]),
% 			before_loop(Host, Path, {Handler, Opts}, Port)
% 	end.

% -spec start_link(inet:socket(), module(), pid())
% 	-> {ok, pid()}.
% start_link(LSocket, Transport, ConnsSup) ->
% 	Pid = spawn_link(?MODULE, loop, [LSocket, Transport, ConnsSup]),
% 	{ok, Pid}.

% -spec loop(inet:socket(), module(), pid()) -> no_return().
% loop(LSocket, Transport, ConnsSup) ->
% 	_ = case Transport:accept(LSocket, infinity) of
% 		{ok, CSocket} ->
% 			case Transport:controlling_process(CSocket, ConnsSup) of
% 				ok ->
% 					%% This call will not return until process has been started
% 					%% AND we are below the maximum number of connections.
% 					ranch_conns_sup:start_protocol(ConnsSup, CSocket);
% 				{error, _} ->
% 					Transport:close(CSocket)
% 			end;
% 		%% Reduce the accept rate if we run out of file descriptors.
% 		%% We can't accept anymore anyway, so we might as well wait
% 		%% a little for the situation to resolve itself.
% 		{error, emfile} ->
% 			receive after 100 -> ok end;
% 		%% We want to crash if the listening socket got closed.
% 		{error, Reason} when Reason =/= closed ->
% 			ok
% 	end,
% 	flush(),
% 	?MODULE:loop(LSocket, Transport, ConnsSup).

% flush() ->
% 	receive Msg ->
% 		error_logger:error_msg(
% 			"Ranch acceptor received unexpected message: ~p~n",
% 			[Msg]),
% 		flush()
% 	after 0 ->
% 		ok
% 	end.
