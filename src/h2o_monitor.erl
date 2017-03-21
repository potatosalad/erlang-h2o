%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  17 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_monitor).
-behaviour(gen_server).

%% Public API
-export([start_link/0]).
-export([stats/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Records
-record(state, {
	avg = 0 :: integer(),
	max = 0 :: integer(),
	min = 0 :: integer(),
	cnt = 0 :: integer()
	% req = #{} :: map()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link()
	-> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stats() ->
	gen_server:call(?MODULE, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([])
	-> ignore | {ok, #state{}} | {stop, any()}.
init([]) ->
	ok = h2o_nif:register_monitor(),
	State = #state{},
	{ok, State}.

-spec handle_call(any(), {pid(), any()}, #state{})
	-> {reply, any(), #state{}}.
handle_call(stats, _From, State=#state{avg=Avg, max=Max, min=Min, cnt=Cnt}) ->
	{reply, [{min, Min}, {max, Max}, {avg, Avg}, {cnt, Cnt}], State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

-spec handle_cast(any(), #state{})
	-> {noreply, #state{}} | {stop, any(), #state{}}.
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), #state{})
	-> {noreply, #state{}}.
handle_info({true, USec}, State=#state{cnt=0}) ->
	Diff = h2o_nif:monitor_ack(USec),
	{noreply, State#state{min=Diff, max=Diff, avg=Diff, cnt=1}};
handle_info({true, USec}, State=#state{min=Min, max=Max, avg=Avg, cnt=Cnt}) ->
	Diff = h2o_nif:monitor_ack(USec),
	Min2 = min(Min, Diff),
	Max2 = max(Max, Diff),
	Avg2 = ((Diff + Avg) div 2) + ((Diff + Avg) rem 2),
	Cnt2 = Cnt + 1,
	% io:format("diff: ~p~n", [Diff]),
	{noreply, State#state{min=Min2, max=Max2, avg=Avg2, cnt=Cnt2}};
% handle_info({true, Req}, State=#state{req=Req0}) ->
% 	Req1 = Req0#{Req => true},
% 	io:format("req: ~p~n", [Req]),
% 	{noreply, State#state{req=Req1}};
% handle_info({false, Req}, State=#state{req=Req0}) ->
% 	case maps:take(Req, Req0) of
% 		{true, Req1} ->
% 			io:format("end: ~p~n", [Req]),
% 			{noreply, State#state{req=Req1}};
% 		error ->
% 			io:format("404: ~p~n", [Req]),
% 			{noreply, State}
% 	end;
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), #state{})
	-> ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(any(), #state{}, any())
	-> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
