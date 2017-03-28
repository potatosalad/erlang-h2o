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
-module(h2o_batch).

%% Public API
-export([start_link/0]).
-export([call/1]).
-export([call/2]).
-export([cast/1]).
% -export([start_link/1]).
% -export([call/2]).
% -export([call/3]).
% -export([cast/2]).

%% Private API
-export([init_it/2]).

%% Records
% -type batch() ::
% 	fun(([any()]) -> ok).

-record(state, {
	parent = undefined :: undefined | pid(),
	% batch  = undefined :: undefined | batch(),
	data   = []        :: [any()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
	proc_lib:start_link(?MODULE, init_it, [self(), ?MODULE]).

call(Request) ->
	call(Request, infinity).

call(Request, Timeout) ->
	try
		Pid = get_pid(?MODULE),
		MonitorRef = erlang:monitor(process, Pid),
		catch erlang:send(Pid, {'$gen_call', {self(), MonitorRef}, Request}, [noconnect]),
		receive
			{MonitorRef, Reply} ->
				true = erlang:demonitor(MonitorRef, [flush]),
				{ok, Reply};
			{'DOWN', MonitorRef, _, _, Reason} ->
				exit(Reason)
		after
			Timeout ->
				true = erlang:demonitor(MonitorRef, [flush]),
				exit(timeout)
		end
	catch
		exit:ExitReason ->
			exit({ExitReason, {?MODULE, call, [Request, Timeout]}})
	end.

cast(Request) ->
	catch erlang:send(?MODULE, {'$gen_cast', Request}, [noconnect]),
	ok.

% start_link(Name) when is_atom(Name) ->
% 	proc_lib:start_link(?MODULE, init_it, [self(), Name]).

% call(Name, Request) ->
% 	call(Name, Request, infinity).

% call(Name, Request, Timeout) ->
% 	try
% 		Pid = get_pid(Name),
% 		MonitorRef = erlang:monitor(process, Pid),
% 		catch erlang:send(Name, {'$gen_call', {self(), MonitorRef}, Request}, [noconnect]),
% 		receive
% 			{MonitorRef, Reply} ->
% 				true = erlang:demonitor(MonitorRef, [flush]),
% 				{ok, Reply};
% 			{'DOWN', MonitorRef, _, _, Reason} ->
% 				exit(Reason)
% 		after
% 			Timeout ->
% 				true = erlang:demonitor(MonitorRef, [flush]),
% 				exit(timeout)
% 		end
% 	catch
% 		exit:ExitReason ->
% 			exit({ExitReason, {?MODULE, call, [Name, Request, Timeout]}})
% 	end.

% cast(Name, Request) ->
% 	catch erlang:send(Name, {'$gen_cast', Request}, [noconnect]),
% 	ok.

%%%===================================================================
%%% Private API
%%%===================================================================

%% @private
init_it(Parent, Name) ->
	case register_name(Name) of
		true ->
			init(Parent);
		{false, Pid} ->
			proc_lib:init_ack(Parent, {error, {already_started, Pid}})
	end.

%% @private
init(Parent) ->
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	State = #state{parent=Parent},
	loop_notify(State).

%% @private
loop_passive(State=#state{data=Data}) ->
	receive
		Request = {'$gen_cast', _} ->
			loop_passive(State#state{data=[Request | Data]});
		Request = {'$gen_call', _, _} ->
			loop_passive(State#state{data=[Request | Data]});
		ready_input ->
			ok = h2o_nif:batch(Data),
			loop_notify(State#state{data=[]});
		Info ->
			error_logger:error_msg(
				"~p (passive) received unexpected message ~p~n",
				[?MODULE, Info]),
			loop_passive(State)
	end.

%% @private
loop_notify(State=#state{data=Data}) ->
	receive
		Request = {'$gen_cast', _} ->
			self() ! ready_input,
			loop_passive(State#state{data=[Request | Data]});
		Request = {'$gen_call', _, _} ->
			self() ! ready_input,
			loop_passive(State#state{data=[Request | Data]});
		Info ->
			error_logger:error_msg(
				"~p (notify) received unexpected message ~p~n",
				[?MODULE, Info]),
			loop_notify(State)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
get_pid(Pid) when is_pid(Pid) ->
	Pid;
get_pid(Atom) when is_atom(Atom) ->
	case erlang:whereis(Atom) of
		Pid when is_pid(Pid) ->
			Pid;
		undefined ->
			exit(noproc)
	end.

%% @private
register_name(Name) ->
	try erlang:register(Name, self()) of
		true -> true
	catch
		error:_ ->
			{false, erlang:whereis(Name)}
	end.
