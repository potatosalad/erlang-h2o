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
-module(h2o_protocol).
-behaviour(ranch_protocol).

-include("h2o_port.hrl").

%% ranch_protocol callbacks
-export([start_link/4]).
-export([init/4]).

%%%===================================================================
%%% ranch_protocol callbacks
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
	% io:format("h2o_protocol(~p, ~p, ~p, ~p)~n", [Ref, Socket, Transport, Opts]),
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, _Transport, []) ->
	ok = ranch:accept_ack(Ref),
	% ok = h2o_req:set_status(Socket, 200),
	% ok = h2o_req:add_header(Socket, <<"content-type">>, <<"text/plain">>),
	% ok = h2o_req:send_inline(Socket, <<"Unknown Error">>),
	ok = h2o_req:reply(Socket, 200, #{<<"content-type">> => <<"text/plain">>}, <<"Plain Text">>),
	% ok = h2o_req:delegate(Socket),
	exit(normal).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
