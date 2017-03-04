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
-export([to_id/1]).

%% Types
-type id() :: integer().
-type ref() :: {id(), binary() | reference()}.
-type obj() :: #h2o_port{}.
-export_type([id/0]).
-export_type([ref/0]).
-export_type([obj/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

open() ->
	Port = #h2o_port{id=ID} = h2o_nif:port_open(),
	true = h2o_kernel:port_connect(ID),
	{ok, Port}.

open(Parent) ->
	Child = #h2o_port{id=ID} = h2o_nif:port_open(Parent),
	true = h2o_kernel:port_connect(ID),
	{ok, Child}.

close(Port) ->
	ID = to_id(Port),
	ok = h2o_nif:port_close(Port),
	receive
		{h2o_port_closed, ID} ->
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
									true = h2o_kernel:port_connect(to_id(Port), NewOwner),
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

to_id(#h2o_port{id=ID}) ->
	ID;
to_id(ID) when is_integer(ID) ->
	ID.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
sync_input(P, Owner, Flag) ->
	sync_input(P, to_id(P), Owner, Flag).

%% @private
sync_input(P, ID, Owner, Flag) ->
	receive
		{h2o_port_data, ID, Data} ->
			Owner ! {h2o_port_data, ID, Data},
			sync_input(P, ID, Owner, Flag);
		{h2o_port_closed, ID} ->
			Owner ! {h2o_port_closed, ID},
			sync_input(P, ID, Owner, true)
	after
		0 ->
			Flag
	end.
