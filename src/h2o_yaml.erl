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

encode(Term) ->
	[encode_term(Term, 0, "---\n"), "\n"].

encode_term(Dict=[{_, _} | _], Level, Acc) ->
	encode_dict(Dict, Level, Acc);
encode_term(List, Level, Acc) when is_list(List) ->
	encode_list(List, Level, Acc);
encode_term(Map, Level, Acc) when is_map(Map) ->
	encode_map(Map, Level, Acc);
encode_term(Scalar, Level, Acc) when ?is_scalar(Scalar) ->
	encode_scalar(Scalar, Level, Acc).

encode_dict(Dict, Level, Acc) ->
	Indent = indent(Level),
	[Acc | [begin
		case V of
			[] ->
				["\n", Indent, encode_scalar(K, 0, []), ": []"];
			_ when (is_list(V) orelse is_map(V)) ->
				["\n", Indent, encode_scalar(K, 0, []), ":", encode_term(V, Level + 1, [])];
			_ when ?is_scalar(V) ->
				["\n", Indent, encode_scalar(K, 0, []), ": ", encode_scalar(V, 0, [])];
			_ ->
				[]
		end
	end || {K, V} <- Dict, ?is_scalar(K)]].

encode_list([], Level, Acc) ->
	Indent = indent(Level),
	[Acc, "\n", Indent, "[]"];
encode_list(List, Level, Acc) ->
	Indent = indent(Level),
	[Acc | [begin
		{Sep, Lvl} = if
			is_list(Element) orelse is_map(Element) ->
				{"-", Level + 1};
			true ->
				{"- ", 0}
		end,
		encode_term(Element, Lvl, ["\n", Indent, Sep])
	end || Element <- List]].

encode_map(Map, Level, Acc) ->
	Indent = indent(Level),
	Folder = fun
		(K, [], A) when ?is_scalar(K) ->
			[A, "\n", Indent, encode_scalar(K, 0, []), ": []"];
		(K, V, A) when ?is_scalar(K) andalso (is_list(V) orelse is_map(V)) ->
			[A, "\n", Indent, encode_scalar(K, 0, []), ":", encode_term(V, Level + 1, [])];
		(K, V, A) when ?is_scalar(K) andalso ?is_scalar(V) ->
			[A, "\n", Indent, encode_scalar(K, 0, []), ": ", encode_scalar(V, 0, [])];
		(_, _, A) ->
			A
	end,
	maps:fold(Folder, Acc, Map).

encode_scalar(Binary, Level, Acc) when is_binary(Binary) ->
	[Acc, indent(Level), $", ?ESCAPE_BIN(Binary), $"];
encode_scalar(Boolean, Level, Acc) when is_boolean(Boolean) ->
	[Acc, indent(Level), atom_to_list(Boolean)];
encode_scalar(Float, Level, Acc) when is_float(Float) ->
	[Acc, indent(Level), float_to_list(Float)];
encode_scalar(Integer, Level, Acc) when is_integer(Integer) ->
	[Acc, indent(Level), integer_to_list(Integer)];
encode_scalar(nil, Level, Acc) ->
	[Acc, indent(Level), "!!null"];
encode_scalar(Reference, Level, Acc) when is_reference(Reference) ->
	[Acc, indent(Level), "!!binary ", $", base64url:encode(erlang:term_to_binary(Reference)), $"].

indent(Level) ->
	binary:copy(<<"  ">>, Level).
