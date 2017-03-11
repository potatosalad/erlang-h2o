%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  02 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_port).

-include("h2o_port.hrl").

%% Public API
-export([open/0]).
-export([open/1]).
-export([close/1]).
-export([controlling_process/2]).
-export([accept/1]).
-export([accept/2]).

%% Types
-type id() :: integer().
-type ref() :: #h2o_port{}.
-export_type([id/0]).
-export_type([ref/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

open() ->
	Port = #h2o_port{} = h2o_nif:port_open(),
	true = h2o_kernel:port_connect(Port),
	{ok, Port}.

open(Parent) ->
	Child = #h2o_port{} = h2o_nif:port_open(Parent),
	true = h2o_kernel:port_connect(Child),
	{ok, Child}.

close(Port) ->
	ok = h2o_nif:port_close(Port),
	receive
		{h2o_port_closed, Port} ->
			ok
	after
		0 ->
			ok
	end.

controlling_process(Port, NewOwner) when is_pid(NewOwner) ->
	case h2o_nif:port_info(Port, connected) of
		{connected, NewOwner} ->
			ok;
		{connected, Pid} when Pid =/= self() ->
			{error, not_owner};
		undefined ->
			{error, einval};
		_ ->
			case h2o_nif:port_getopt(Port, active) of
				{ok, A0} ->
					SetOptRes =
						case A0 of
							false -> ok;
							_ -> h2o_nif:port_setopt(Port, active, false)
						end,
					case {sync_input(Port, NewOwner, false), SetOptRes} of
						{true, _} -> %% port already closed
							ok;
						{false, ok} ->
							try h2o_nif:port_connect(Port, NewOwner) of
								true ->
									true = h2o_kernel:port_connect(Port, NewOwner),
									case A0 of
										false -> ok;
										_ -> h2o_nif:port_setopt(Port, active, A0)
									end
							catch
								error:Reason ->
									{error, Reason}
							end;
						{false, Error} ->
							Error
					end;
				Error ->
					Error
			end
	end.

accept(LPort) ->
	case h2o_nif:port_accept(LPort) of
		{ok, APort} ->
			true = h2o_kernel:port_connect(APort),
			{ok, APort};
		{accept, AsyncID} ->
			receive
				{accept, AsyncID, APort} ->
					true = h2o_kernel:port_connect(APort),
					{ok, APort}
			end;
		AcceptError ->
			AcceptError
	end.

accept(LPort, infinity) ->
	accept(LPort);
accept(LPort, Timeout) when is_integer(Timeout) andalso Timeout >= 0 ->
	Parent = self(),
	Ref = erlang:make_ref(),
	{Pid, MonitorRef} = spawn_monitor(fun() ->
		case h2o_nif:port_accept(LPort) of
			{ok, APort} ->
				true = h2o_kernel:port_connect(APort),
				ok = controlling_process(APort, Parent),
				Parent ! {shoot, Ref, APort},
				receive
					{ack, Ref} ->
						exit(normal)
				end;
			{accept, AsyncID} ->
				receive
					{accept, AsyncID, APort} ->
						true = h2o_kernel:port_connect(APort),
						Parent ! {shoot, Ref, APort},
						ok = controlling_process(APort, Parent),
						receive
							{ack, Ref} ->
								exit(normal)
						end
				after
					Timeout ->
						Parent ! {timeout, Ref},
						exit(normal)
				end;
			AcceptError ->
				Parent ! {error, Ref, AcceptError},
				exit(normal)
		end
	end),
	receive
		{shoot, Ref, APort} ->
			Pid ! {ack, Ref},
			receive
				{'DOWN', MonitorRef, process, Pid, normal} ->
					{ok, APort}
			end;
		{timeout, Ref} ->
			receive
				{'DOWN', MonitorRef, process, Pid, normal} ->
					{error, timeout}
			end;
		{error, Ref, AcceptError} ->
			receive
				{'DOWN', MonitorRef, process, Pid, normal} ->
					AcceptError
			end;
		{'DOWN', MonitorRef, process, Pid, Reason} ->
			{error, Reason}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
sync_input(P, Owner, Flag) ->
	receive
		{h2o_port_data, P, Data} ->
			Owner ! {h2o_port_data, P, Data},
			sync_input(P, Owner, Flag);
		{h2o_port_closed, P} ->
			Owner ! {h2o_port_closed, P},
			sync_input(P, Owner, true)
	after
		0 ->
			Flag
	end.
