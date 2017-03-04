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

-export([start/0]).
% -export([server_open/0]).
% -export([server_getcfg/1]).
% -export([server_setcfg/2]).
% -export([server_getstatus/1]).
% -export([server_start/1]).
% -export([handler_accept/1]).
% -export([handler_accept/2]).
% -export([request_reply/1]).
% -export([get_handler/1]).

start() ->
	application:ensure_all_started(?MODULE).

% server_open() ->
% 	h2o_nif:server_open().

% server_getcfg(Server) ->
% 	h2o_nif:server_getcfg(Server).

% server_setcfg(Server, Config) ->
% 	h2o_nif:server_setcfg(Server, h2o_yaml:encode(Config)).

% server_getstatus(Server) ->
% 	h2o_nif:server_getstatus(Server).

% server_start(Server) ->
% 	h2o_nif:server_start(Server).

% handler_accept(Handler) ->
% 	handler_accept(Handler, infinity).

% handler_accept(Handler, Timeout) ->
% 	case h2o_nif:handler_accept(Handler, Timeout) of
% 		{AsyncID} ->
% 			io:format("async request~n"),
% 			receive
% 				{AsyncID, Request} ->
% 					{ok, Request}
% 			end;
% 		Request ->
% 			io:format("sync request~n"),
% 			{ok, Request}
% 	end.

% request_reply(Tag) ->
% 	receive
% 		{h2o_request, Tag, Request} ->
% 			io:format("got request~n"),
% 			h2o_nif:request_reply(Request)
% 	end.

% get_handler(Tag) ->
% 	receive
% 		{h2o_handler, Tag, Handler} ->
% 			{ok, Handler}
% 	end.

% receive_handlers(Server, Acc) ->
% 	receive
% 		{h2o_handler, Server, Handler} ->
% 			receive_handlers(Server, [{}])
