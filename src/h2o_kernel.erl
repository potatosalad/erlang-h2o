%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  16 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_kernel).
-behaviour(gen_server).

%% Public API
-export([start_link/0]).
-export([port_connect/1]).
-export([port_connect/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-type monitors() :: [{{reference(), pid()}, h2o_port:id()}].

%% Records
-record(state, {
	monitors = [] :: monitors()
}).

%% Macros
-define(TAB, ?MODULE).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link()
	-> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec port_connect(h2o_port:id())
	-> true.
port_connect(PortID) ->
	true = gen_server:call(?MODULE, {port_connect, PortID, self()}),
	true.

-spec port_connect(h2o_port:id(), pid())
	-> true.
port_connect(PortID, NewOwner) ->
	true = gen_server:call(?MODULE, {port_connect, PortID, self(), NewOwner}),
	true.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([])
	-> ignore | {ok, #state{}} | {stop, any()}.
init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Port} || [Port, Pid] <- ets:match(?TAB, {{port, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.

-spec handle_call(any(), {pid(), any()}, #state{})
	-> {reply, any(), #state{}}.
handle_call({port_connect, Port, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {{port, Port}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			Monitors2 = [{{MonitorRef, Pid}, Port} | Monitors],
			{reply, true, State#state{monitors=Monitors2}};
		false ->
			{reply, false, State}
	end;
handle_call({port_connect, Port, Pid, NewOwner}, _From, State=#state{monitors=Monitors}) ->
	case ets:take(?TAB, {port, Port}) of
		[{{port, Port}, Pid}] ->
			true = ets:insert_new(?TAB, {{port, Port}, NewOwner}),
			MonitorRef = erlang:monitor(process, NewOwner),
			Monitors2 = [{{MonitorRef, NewOwner}, Port} | disconnect(Monitors, Port, Pid)],
			{reply, true, State#state{monitors=Monitors2}};
		[KeyVal] ->
			true = ets:insert_new(?TAB, KeyVal),
			{reply, false, State};
		[] ->
			{reply, false, State}
	end;
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
handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
	case lists:keytake({MonitorRef, Pid}, 1, Monitors) of
		{value, {_, Port}, Monitors2} ->
			true = ets:delete(?TAB, {port, Port}),
			ok = h2o_port:close(Port),
			{noreply, State#state{monitors=Monitors2}};
		false ->
			{noreply, State}
	end;
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

%% @private
disconnect(Monitors, Port, Pid) ->
	disconnect(Monitors, Port, Pid, []).

%% @private
disconnect([{{MonitorRef, Pid}, Port} | Monitors], Port, Pid, Head) ->
	true = erlang:demonitor(MonitorRef, [flush]),
	lists:reverse(Head, Monitors);
disconnect([Monitor | Monitors], Port, Pid, Head) ->
	disconnect(Monitors, Port, Pid, [Monitor | Head]);
disconnect([], _Port, _Pid, Head) ->
	lists:reverse(Head).
