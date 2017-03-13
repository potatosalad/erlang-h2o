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
-module(h2o_req).

-include("h2o_port.hrl").

%% Public API
-export([info/0]).
-export([add_header/3]).
-export([delegate/1]).
-export([send_inline/2]).
-export([set_status/2]).

%% Response API
-export([reply/4]).

%% Macros
-define(call_close(R, F),
	case (F) of
		ok ->
			h2o_port:close(R);
		Error ->
			Error
	end).

%%%===================================================================
%%% Public API
%%%===================================================================

info() ->
	[begin
		{Key, [begin
			{Type, {Sec, (NSec div 1000) + (NSec rem 1000)}}
		end || {Type, {Sec, NSec}} <- Stat]}
	end || {Key, Stat} <- h2o_nif:request_info()].

add_header(Request, Name, Value) ->
	h2o_nif:request_add_header(Request, Name, Value).

delegate(Request) ->
	?call_close(Request,
		h2o_nif:request_delegate(Request)).

send_inline(Request, Body) ->
	case h2o_nif:request_send_inline(Request, Body) of
		ok ->
			h2o_port:close(Request);
			% ok;
		Error ->
			Error
	end.

set_status(Request, Status) ->
	h2o_nif:request_set_status(Request, Status).

%%%===================================================================
%%% Public API
%%%===================================================================

reply(Request, Status, Headers, Body) ->
	?call_close(Request,
		h2o_nif:request_reply(Request, Status, Headers#{
			<<"content-length">> => integer_to_binary(iolist_size(Body))
		}, Body)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
