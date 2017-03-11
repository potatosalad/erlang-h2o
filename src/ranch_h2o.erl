%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  10 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(ranch_h2o).
-behaviour(ranch_transport).

-include("h2o_port.hrl").

%% ranch_transport callbacks
-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([accept_ack/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).

%%%===================================================================
%%% ranch_transport callbacks
%%%===================================================================

name() ->
	h2o.

secure() ->
	true.

messages() ->
	{h2o_port_data, h2o_port_closed, h2o_port_error}.

listen([{socket, HPort=#h2o_port{}}]) ->
	case h2o_nif:port_info(HPort, state) of
		{state, State} when (State band ?H2O_NIF_PORT_STATE_LISTENING) == ?H2O_NIF_PORT_STATE_LISTENING ->
			{ok, HPort};
		{state, State} ->
			{error, {badstate, State}};
		Error ->
			Error
	end.

disallowed_listen_options() ->
	[].

accept(LSocket, Timeout) ->
	h2o_port:accept(LSocket, Timeout).

accept_ack(_CSocket, _Timeout) ->
	ok.

connect(_Host, _Port, _Opts) ->
	erlang:error(notsup).

connect(_Host, _Port, _Opts, _Timeout) ->
	erlang:error(notsup).

recv(_Socket, _Length, _Timeout) ->
	erlang:error(notsup).

send(_Socket, _Packet) ->
	erlang:error(notsup).

sendfile(_Socket, _Filename) ->
	erlang:error(notsup).

sendfile(_Socket, _File, _Offset, _Bytes) ->
	erlang:error(notsup).

sendfile(_Socket, _Filename, _Offset, _Bytes, _Opts) ->
	erlang:error(notsup).

setopts(_Socket, _Opts) ->
	erlang:error(notsup).

controlling_process(Socket, Pid) ->
	h2o_port:controlling_process(Socket, Pid).

peername(_Socket) ->
	erlang:error(notsup).

sockname(_Socket) ->
	{ok, {{127,0,0,1}, 0}}.

shutdown(_Socket, _How) ->
	erlang:error(notsup).

close(Socket) ->
	h2o_port:close(Socket).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
