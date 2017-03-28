%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  22 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_handler_event).

-include("h2o_req.hrl").

%% Public API
% -export([start_link/0]).
-export([ack/2]).
-export([reply/4]).

%% Private API
% -export([init_it/1]).

% %% Records
% -record(state, {
% 	parent = undefined :: undefined | pid(),
% 	name   = notify    :: notify | passive,
% 	data   = []        :: [any()]
% }).

%%%===================================================================
%%% Public API
%%%===================================================================

% start_link() ->
% 	h2o_batch:start_link(?MODULE).
	% proc_lib:start_link(?MODULE, init_it, [self()]).

ack(Parent, Event) ->
	receive
		{shoot, Parent, Event} ->
			ok
	end.

% reply(#{event := Event}, Status, Headers, Body) ->
% 	reply(Event, Status, Headers, Body);
% reply([{event, Event} | _], Status, Headers, Body) ->
% 	reply(Event, Status, Headers, Body);
reply(#h2o_req{event=Event}, Status, Headers, Body) ->
	% reply(Event, Status, Headers, Body);
% reply(Event, Status, Headers, Body) ->
	h2o_batch:cast({handler_event_reply, Event, Status, Headers, Body}).
	% ?MODULE ! {'$gen_cast', {reply, Event, Status, Headers, Body}},
	% ok.

% %%%===================================================================
% %%% Private API
% %%%===================================================================

% %% @private
% init_it(Parent) ->
% 	case register_name(?MODULE) of
% 		true ->
% 			init(Parent);
% 		{false, Pid} ->
% 			proc_lib:init_ack(Parent, {error, {already_started, Pid}})
% 	end.

% %% @private
% init(Parent) ->
% 	ok = proc_lib:init_ack(Parent, {ok, self()}),
% 	State = #state{parent=Parent},
% 	loop(State).

% %% @private
% loop(State=#state{name=passive, data=Data}) ->
% 	receive
% 		{'$gen_cast', Msg = {reply, _, _, _, _}} ->
% 			loop(State#state{data=[Msg | Data]});
% 		{'$gen_cast', ready_input} ->
% 			% io:format("data = ~p~n", [length(Data)]),
% 			ok = h2o_nif:handler_event_batch(Data),
% 			loop(State#state{name=notify, data=[]});
% 		Info ->
% 			error_logger:error_msg(
% 				"~p (passive) received unexpected message ~p~n",
% 				[?MODULE, Info]),
% 			loop(State)
% 	end;
% loop(State=#state{name=notify, data=Data}) ->
% 	receive
% 		{'$gen_cast', Msg = {reply, _, _, _, _}} ->
% 			self() ! {'$gen_cast', ready_input},
% 			loop(State#state{name=passive, data=[Msg | Data]});
% 		Info ->
% 			error_logger:error_msg(
% 				"~p (notify) received unexpected message ~p~n",
% 				[?MODULE, Info]),
% 			loop(State)
% 	end.

% %%%-------------------------------------------------------------------
% %%% Internal functions
% %%%-------------------------------------------------------------------

% %% @private
% register_name(Name) ->
% 	try erlang:register(Name, self()) of
% 		true -> true
% 	catch
% 		error:_ ->
% 			{false, erlang:whereis(Name)}
% 	end.
