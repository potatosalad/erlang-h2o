%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc A very simplistic/incomplete YAML encoder.
%%%
%%% @end
%%% Created :  21 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_yaml).

%% Public API
-export([encode/1]).

%% Macros
-define(is_nil(T),
	(T == nil)).
-define(is_scalar(T),
	(is_binary(T) orelse is_boolean(T) orelse is_float(T) orelse is_integer(T) orelse ?is_nil(T) orelse is_reference(T))).
-define(INT_TO_HEX(C),
	case C of
		16#0 -> $0;
		16#1 -> $1;
		16#2 -> $2;
		16#3 -> $3;
		16#4 -> $4;
		16#5 -> $5;
		16#6 -> $6;
		16#7 -> $7;
		16#8 -> $8;
		16#9 -> $9;
		16#A -> $A;
		16#B -> $B;
		16#C -> $C;
		16#D -> $D;
		16#E -> $E;
		16#F -> $F
	end).
-define(ESCAPE_CHAR(C),
	case C of
		_ when C >= $\s andalso C =< $~ ->
			<< C >>;
		_ ->
			<< $\\, $x, (?INT_TO_HEX(C bsr 4)), (?INT_TO_HEX(C band 16#F)) >>
	end).
-define(ESCAPE_BIN(B),
	<< ?ESCAPE_CHAR(C) || << C >> <= B >>).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec encode(any()) -> {iodata(), any()}.
encode(Term) ->
	{YAML, Bindings} = encode_term(Term, 0, "---\n", []),
	{[YAML, $\n], lists:reverse(Bindings)}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
encode_term(Dict=[{_, _} | _], Level, Acc, Bindings) ->
	encode_dict(Dict, Level, Acc, Bindings);
encode_term(List, Level, Acc, Bindings) when is_list(List) ->
	encode_list(List, Level, Acc, Bindings);
encode_term(Map, Level, Acc, Bindings) when is_map(Map) ->
	encode_map(Map, Level, Acc, Bindings);
encode_term(Scalar, Level, Acc, Bindings) when ?is_scalar(Scalar) ->
	encode_scalar(Scalar, Level, Acc, Bindings).

%% @private
encode_dict(Dict, Level, Acc, Bindings) ->
	encode_dict(Dict, indent(Level), Level, Acc, Bindings).

%% @private
encode_dict([{K, V} | Dict], Indent, Level, Acc0, Bindings0) ->
	{Acc1, Bindings1} = encode_dict_term(K, V, Indent, Level, Acc0, Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Bindings1);
encode_dict([], _Indent, _Level, Acc, Bindings) ->
	{Acc, Bindings};
encode_dict(List, _Indent, _Level, _Acc, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_term(K0, [], Indent, _Level, Acc0, Bindings0) when ?is_scalar(K0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, $[, $]],
	{Acc1, Bindings1};
encode_dict_term(K0 = <<"hosts">>, V0, Indent, Level, Acc0, Bindings0) when (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_dict_hosts(V0, Level + 1, [], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	{Acc1, Bindings2};
encode_dict_term(K0, V0, Indent, Level, Acc0, Bindings0) when ?is_scalar(K0) andalso (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_term(V0, Level + 1, [], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	{Acc1, Bindings2};
encode_dict_term(K0, V0, Indent, _Level, Acc0, Bindings0) when ?is_scalar(K0) andalso ?is_scalar(V0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_scalar(V0, 0, [], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	{Acc1, Bindings2};
encode_dict_term(K, V, _Indent, _Level, _Acc, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_dict_hosts(Map, Level, Acc, Bindings) when is_map(Map) ->
	encode_dict_hosts(maps:to_list(Map), Level, Acc, Bindings);
encode_dict_hosts(Dict=[{_, _} | _], Level, Acc, Bindings) ->
	encode_dict_hosts(Dict, indent(Level), Level, Acc, Bindings);
encode_dict_hosts(List, _Level, _Acc, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_hosts([{K0, V0} | Dict], Indent, Level, Acc0, Bindings0) when ?is_scalar(K0) andalso (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_dict_host(V0, Level + 1, [], [K0], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	encode_dict_hosts(Dict, Indent, Level, Acc1, Bindings2);
encode_dict_hosts([], _Indent, _Level, Acc, Bindings) ->
	{Acc, Bindings};
encode_dict_hosts(List, _Indent, _Level, _Acc, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_host(Map, Level, Acc, Binding, Bindings) when is_map(Map) ->
	encode_dict_host(maps:to_list(Map), Level, Acc, Binding, Bindings);
encode_dict_host(Dict=[{_, _} | _], Level, Acc, Binding, Bindings) ->
	encode_dict_host(Dict, indent(Level), Level, Acc, Binding, Bindings);
encode_dict_host(List, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_host([{K0 = <<"paths">>, V0} | Dict], Indent, Level, Acc0, Binding, Bindings0) when ?is_scalar(K0) andalso (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_dict_paths(V0, Level + 1, [], Binding, Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	encode_dict_host(Dict, Indent, Level, Acc1, Binding, Bindings2);
encode_dict_host([{K, V} | Dict], Indent, Level, Acc0, Binding, Bindings0) ->
	{Acc1, Bindings1} = encode_dict_term(K, V, Indent, Level, Acc0, Bindings0),
	encode_dict_host(Dict, Indent, Level, Acc1, Binding, Bindings1);
encode_dict_host([], _Indent, _Level, Acc, _Binding, Bindings) ->
	{Acc, Bindings};
encode_dict_host(List, _Indent, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_paths(Map, Level, Acc, Binding, Bindings) when is_map(Map) ->
	encode_dict_paths(maps:to_list(Map), Level, Acc, Binding, Bindings);
encode_dict_paths(Dict=[{_, _} | _], Level, Acc, Binding, Bindings) ->
	encode_dict_paths(Dict, indent(Level), Level, Acc, Binding, Bindings);
encode_dict_paths(List, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_paths([{K0, V0} | Dict], Indent, Level, Acc0, Binding, Bindings0) when ?is_scalar(K0) andalso (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_dict_path(V0, Level + 1, [], [K0 | Binding], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	encode_dict_paths(Dict, Indent, Level, Acc1, Binding, Bindings2);
encode_dict_paths([], _Indent, _Level, Acc, _Binding, Bindings) ->
	{Acc, Bindings};
encode_dict_paths(List, _Indent, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_path(Map, Level, Acc, Binding, Bindings) when is_map(Map) ->
	encode_dict_path(maps:to_list(Map), Level, Acc, Binding, Bindings);
encode_dict_path(Dict=[{_, _} | _], Level, Acc, Binding, Bindings) ->
	encode_dict_path(Dict, indent(Level), Level, Acc, Binding, Bindings);
encode_dict_path(List, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_path([{K0, Value} | Dict], Indent, Level, Acc0, Binding, Bindings0)
		when (K0 == <<"erlang.filter">>
			orelse K0 == <<"erlang.handler">>
			orelse K0 == <<"erlang.logger">>
			orelse K0 == <<"erlang.websocket">>)
		andalso is_tuple(Value)
		andalso (tuple_size(Value) == 2 orelse tuple_size(Value) == 3 orelse tuple_size(Value) == 4) ->
	{Handler, Opts, SupType, NbAcceptors} = case Value of
		{Handler0, Opts0} ->
			{Handler0, Opts0, worker, 10};
		{Handler0, Opts0, SupType0} when SupType0 == worker orelse SupType0 == supervisor ->
			{Handler0, Opts0, SupType0, 10};
		{Handler0, Opts0, SupType0, NbAcceptors0} when (SupType0 == worker orelse SupType0 == supervisor) andalso (is_integer(NbAcceptors0) andalso NbAcceptors0 > 0) ->
			{Handler0, Opts0, SupType0, NbAcceptors0}
	end,
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Ref = erlang:make_ref(),
	{V1, Bindings2} = encode_scalar(Ref, 0, [], Bindings1),
	Type = handler_type(K0),
	Bindings3 = [list_to_tuple(lists:reverse([Ref, NbAcceptors, SupType, Opts, Handler, Type | Binding])) | Bindings2],
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	encode_dict_path(Dict, Indent, Level, Acc1, Binding, Bindings3);
encode_dict_path([{K, V} | Dict], Indent, Level, Acc0, Binding, Bindings0) ->
	{Acc1, Bindings1} = encode_dict_term(K, V, Indent, Level, Acc0, Bindings0),
	encode_dict_path(Dict, Indent, Level, Acc1, Binding, Bindings1);
encode_dict_path([], _Indent, _Level, Acc, _Binding, Bindings) ->
	{Acc, Bindings};
encode_dict_path(List, _Indent, _Level, _Acc, _Binding, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_list([], Level, Acc, Bindings) ->
	Indent = indent(Level),
	{[Acc, $\n, Indent, $[, $]], Bindings};
encode_list(List, Level, Acc, Bindings) ->
	encode_list(List, indent(Level), Level, Acc, Bindings).

%% @private
encode_list([Element | List], Indent, Level, Acc, Bindings) when is_list(Element) orelse is_map(Element) ->
	{Acc2, Bindings2} = encode_term(Element, Level + 1, [Acc, $\n, Indent, $-], Bindings),
	encode_list(List, Indent, Level, Acc2, Bindings2);
encode_list([Element | List], Indent, Level, Acc, Bindings) when ?is_scalar(Element) ->
	{Acc2, Bindings2} = encode_scalar(Element, 0, [Acc, $\n, Indent, $-, $\s], Bindings),
	encode_list(List, Indent, Level, Acc2, Bindings2);
encode_list([], _Indent, _Level, Acc, Bindings) ->
	{Acc, Bindings};
encode_list(List, _Indent, _Level, _Acc, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_map(Map, Level, Acc, Bindings) ->
	encode_dict(maps:to_list(Map), Level, Acc, Bindings).

%% @private
encode_scalar(Binary, Level, Acc, Bindings) when is_binary(Binary) ->
	{[Acc, indent(Level), $", ?ESCAPE_BIN(Binary), $"], Bindings};
encode_scalar(Boolean, Level, Acc, Bindings) when is_boolean(Boolean) ->
	{[Acc, indent(Level), atom_to_list(Boolean)], Bindings};
encode_scalar(Float, Level, Acc, Bindings) when is_float(Float) ->
	{[Acc, indent(Level), float_to_list(Float)], Bindings};
encode_scalar(Integer, Level, Acc, Bindings) when is_integer(Integer) ->
	{[Acc, indent(Level), integer_to_list(Integer)], Bindings};
encode_scalar(nil, Level, Acc, Bindings) ->
	{[Acc, indent(Level), $!, $!, $n, $u, $l, $l], Bindings};
encode_scalar(Reference, Level, Acc, Bindings) when is_reference(Reference) ->
	{[Acc, indent(Level), $!, $!, $b, $i, $n, $a, $r, $y, $\s, $", h2o_nif:string_base64_encode(erlang:term_to_binary(Reference), true), $"], Bindings}.

%% @private
indent(Level) ->
	binary:copy(<<"  ">>, Level).

%% @private
handler_type(<<"erlang.filter">>) -> h2o_filter;
handler_type(<<"erlang.handler">>) -> h2o_handler;
handler_type(<<"erlang.logger">>) -> h2o_logger;
handler_type(<<"erlang.websocket">>) -> h2o_websocket;
handler_type(BadType) -> erlang:error({badarg, [BadType]}).
