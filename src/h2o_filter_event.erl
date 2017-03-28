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
-module(h2o_filter_event).

-include("h2o_batch.hrl").
-include("h2o_req.hrl").

%% Public API
-export([read/1]).
-export([read_entity/2]).
-export([read_start/1]).
-export([send/3]).
-export([start_response/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

read(#h2o_req{event=Event, event_type=filter}) ->
	h2o_nif:filter_event_read(Event).

read_entity(#h2o_req{event=Event, event_type=filter}, Length) ->
	h2o_batch:cast({?H2O_BATCH_filter_event_read_entity, {Event, self(), Length}}).

read_start(#h2o_req{event=Event, event_type=filter}) ->
	h2o_nif:filter_event_read_start(Event).

send(#h2o_req{send_state=final}, _IsFin, _Output) ->
	erlang:error(function_clause);
send(Req=#h2o_req{send_state=in_progress}, IsFin, Output) ->
	?MODULE:send(start_response(Req), IsFin, Output);
send(Req=#h2o_req{send_state=headers, event=Event, event_type=filter}, IsFin=nofin, Output) ->
	ok = h2o_batch:cast({?H2O_BATCH_filter_event_send, {Event, IsFin, Output}}),
	Req;
send(Req=#h2o_req{send_state=headers, event=Event, event_type=filter}, IsFin=fin, Output) ->
	ok = h2o_batch:cast({?H2O_BATCH_filter_event_send, {Event, IsFin, Output}}),
	Req#h2o_req{send_state=final}.

start_response(#h2o_req{send_state=final}) ->
	erlang:error(function_clause);
start_response(#h2o_req{send_state=headers}) ->
	erlang:error(function_clause);
start_response(Req=#h2o_req{send_state=in_progress, event=Event, event_type=filter, res=Res=#h2o_res{}}) ->
	Empty = #h2o_res{},
	Response = h2o_res:response(Res),
	ok = case Response of
		Empty ->
			ok;
		_ ->
			h2o_batch:cast({?H2O_BATCH_filter_event_start_response, {Event, Response}})
	end,
	Req#h2o_req{send_state=headers, res=Empty}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
