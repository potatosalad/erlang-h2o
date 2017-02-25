%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  24 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o).

-export([server_open/0]).
-export([server_getcfg/1]).
-export([server_setcfg/2]).
-export([server_getstatus/1]).
-export([server_start/1]).
-export([request_reply/1]).

server_open() ->
	h2o_nif:server_open().

server_getcfg(Server) ->
	h2o_nif:server_getcfg(Server).

server_setcfg(Server, Config) ->
	h2o_nif:server_setcfg(Server, h2o_yaml:encode(Config)).

server_getstatus(Server) ->
	h2o_nif:server_getstatus(Server).

server_start(Server) ->
	h2o_nif:server_start(Server).

request_reply(Tag) ->
	receive
		{h2o_request, Tag, Request} ->
			io:format("got request~n"),
			h2o_nif:request_reply(Request)
	end.

% request_reply(Request) ->
% 	h2o_nif:request_reply(Request).
