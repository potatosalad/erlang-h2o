%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  27 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(toppage_filter).

-include("h2o_batch.hrl").
-include("h2o_req.hrl").

%% API
-export([on_setup_ostream/2]).
-export([on_ready_input/1]).
-export([on_input/2]).
-export([on_final_input/1]).
% -export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

% on_setup_ostream(Req, []) ->
% 	Pid = proc_lib:spawn_link(?MODULE, init, [Req]),
% 	{ok, Pid}.

% on_ready_input(Pid) ->
% 	Pid ! on_ready_input,
% 	{ok, Pid}.

% on_final_input(Pid) ->
% 	Pid ! on_final_input,
% 	ok.

% init(Req) ->
% 	loop(Req, []).

% loop(Req=#h2o_req{event=Event}, Acc) ->
% 	receive
% 		on_ready_input ->
% 			{ok, Input} = h2o_batch:call({?H2O_BATCH_filter_event_read, {Event}}),
% 			% Input = h2o_nif:filter_event_read(Event),
% 			loop(Req, [Acc, Input]);
% 		on_final_input ->
% 			ok = h2o_batch:cast({?H2O_BATCH_filter_event_send, {Event, fin, Acc}}),
% 			exit(normal)
% 	end.

on_setup_ostream(Req, []) ->
	% io:format("on_setup_ostream: ~p~n", [Req]),
	{ok, Req}.

on_ready_input(Req) ->
	Input = h2o_filter_event:read(Req),
	on_input(Input, Req).
	% NewReq = h2o_filter_event:send(Req, nofin, h2o_nif:string_strtoupper(Input)),
	% {ok, Req}.

on_input(Input, Req) ->
	% io:format("on_input: ~p~n", [Input]),
	% {ok, Req}.
	NewReq = h2o_filter_event:send(Req, nofin, Input),
	{ok, NewReq}.

on_final_input(Req) ->
	% io:format("on_final_input: ~p~n", [Req]),
	_ = h2o_filter_event:send(Req, fin, <<>>),
	ok.
