%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc A very simplistic/incomplete YAML encoder for h2o config.
%%%
%%% @end
%%% Created :  18 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_config).

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
		$" ->
			<< $\\, $" >>;
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
encode(Map) when is_map(Map) ->
	encode(maps:to_list(Map));
encode(List) when is_list(List) ->
	{YAML, Bindings} = encode_dict(List, 0, "---\n", [], []),
	{[YAML, $\n], lists:reverse(Bindings)}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
encode_dict(Dict, Level, Acc, Path, Bindings) ->
	encode_dict(Dict, indent(Level), Level, Acc, Path, Bindings).

%% @private
encode_dict([{K = <<"erlang.filter">>, V} | Dict], Indent, Level, Acc0, Path, Bindings0) ->
	{Acc1, Bindings1} = encode_h2o_filter(K, V, Indent, Level, Acc0, [K | Path], Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Path, Bindings1);
encode_dict([{K = <<"erlang.handler">>, V} | Dict], Indent, Level, Acc0, Path, Bindings0) ->
	{Acc1, Bindings1} = encode_h2o_handler(K, V, Indent, Level, Acc0, [K | Path], Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Path, Bindings1);
encode_dict([{K = <<"erlang.logger">>, V} | Dict], Indent, Level, Acc0, Path, Bindings0) ->
	{Acc1, Bindings1} = encode_h2o_logger(K, V, Indent, Level, Acc0, [K | Path], Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Path, Bindings1);
encode_dict([{K = <<"erlang.websocket">>, V} | Dict], Indent, Level, Acc0, Path, Bindings0) ->
	{Acc1, Bindings1} = encode_h2o_websocket(K, V, Indent, Level, Acc0, [K | Path], Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Path, Bindings1);
encode_dict([{K, V} | Dict], Indent, Level, Acc0, Path, Bindings0) ->
	{Acc1, Bindings1} = encode_dict_term(K, V, Indent, Level, Acc0, [K | Path], Bindings0),
	encode_dict(Dict, Indent, Level, Acc1, Path, Bindings1);
encode_dict([], _Indent, _Level, Acc, _Path, Bindings) ->
	{Acc, Bindings};
encode_dict(List, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_dict_term(K0, [], Indent, _Level, Acc0, _Path, Bindings0) when ?is_scalar(K0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, $[, $]],
	{Acc1, Bindings1};
encode_dict_term(K0, V0, Indent, Level, Acc0, Path, Bindings0) when ?is_scalar(K0) andalso (is_list(V0) orelse is_map(V0)) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_term(V0, Level + 1, [], Path, Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	{Acc1, Bindings2};
encode_dict_term(K0, V0, Indent, _Level, Acc0, _Path, Bindings0) when ?is_scalar(K0) andalso ?is_scalar(V0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	{V1, Bindings2} = encode_scalar(V0, 0, [], Bindings1),
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	{Acc1, Bindings2};
encode_dict_term(K, V, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K], Bindings) ->
	% global
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K, _, <<"hosts">>], Bindings) ->
	% host
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% path
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>], Bindings) ->
	% extension (global)
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>, _, <<"hosts">>], Bindings) ->
	% extension (host)
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K = <<"erlang.filter">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% extension (path)
	make_h2o_filter(K, make_h2o_filter(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_filter(K, V, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_h2o_handler(K = <<"erlang.handler">>, V, Indent, Level, Acc, Path = [K, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% path
	make_h2o_handler(K, make_h2o_handler(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_handler(K, V, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K], Bindings) ->
	% global
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K, _, <<"hosts">>], Bindings) ->
	% host
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% path
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>], Bindings) ->
	% extension (global)
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>, _, <<"hosts">>], Bindings) ->
	% extension (host)
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K = <<"erlang.logger">>, V, Indent, Level, Acc, Path = [K, <<"file.custom-handler">>, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% extension (path)
	make_h2o_logger(K, make_h2o_logger(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_logger(K, V, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_h2o_websocket(K = <<"erlang.websocket">>, V, Indent, Level, Acc, Path = [K, _, <<"paths">>, _, <<"hosts">>], Bindings) ->
	% path
	make_h2o_websocket(K, make_h2o_websocket(V), Indent, Level, Acc, Path, Bindings);
encode_h2o_websocket(K, V, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [K, V]}).

%% @private
encode_list([], Level, Acc, _Path, Bindings) ->
	Indent = indent(Level),
	{[Acc, $\n, Indent, $[, $]], Bindings};
encode_list(List, Level, Acc, Path, Bindings) ->
	encode_list(List, indent(Level), Level, Acc, Path, Bindings).

%% @private
encode_list([Element | List], Indent, Level, Acc, Path, Bindings) when is_list(Element) orelse is_map(Element) ->
	{Acc2, Bindings2} = encode_term(Element, Level + 1, [Acc, $\n, Indent, $-], Path, Bindings),
	encode_list(List, Indent, Level, Acc2, Bindings2);
encode_list([Element | List], Indent, Level, Acc, Path, Bindings) when ?is_scalar(Element) ->
	{Acc2, Bindings2} = encode_scalar(Element, 0, [Acc, $\n, Indent, $-, $\s], Bindings),
	encode_list(List, Indent, Level, Acc2, Path, Bindings2);
encode_list([], _Indent, _Level, Acc, _Path, Bindings) ->
	{Acc, Bindings};
encode_list(List, _Indent, _Level, _Acc, _Path, _Bindings) ->
	erlang:error({badarg, [List]}).

%% @private
encode_map(Map, Level, Acc, Path, Bindings) ->
	encode_dict(maps:to_list(Map), Level, Acc, Path, Bindings).

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
encode_term(Dict=[{_, _} | _], Level, Acc, Path, Bindings) ->
	encode_dict(Dict, Level, Acc, Path, Bindings);
encode_term(List, Level, Acc, Path, Bindings) when is_list(List) ->
	encode_list(List, Level, Acc, Path, Bindings);
encode_term(Map, Level, Acc, Path, Bindings) when is_map(Map) ->
	encode_map(Map, Level, Acc, Path, Bindings);
encode_term(Scalar, Level, Acc, _Path, Bindings) when ?is_scalar(Scalar) ->
	encode_scalar(Scalar, Level, Acc, Bindings).

%% @private
indent(Level) ->
	binary:copy(<<"  ">>, Level).

%% @private
make_h2o_filter({Module, Opts}) when is_atom(Module) ->
	{Module, Opts};
make_h2o_filter(Term) ->
	erlang:error({badarg, [Term]}).

%% @private
make_h2o_filter(K0, V, Indent, _Level, Acc0, Path, Bindings0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Ref = erlang:make_ref(),
	{V1, Bindings2} = encode_scalar(Ref, 0, [], Bindings1),
	Binding = {h2o_filter, Ref, lists:reverse(Path), V},
	Bindings3 = [Binding | Bindings2],
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	{Acc1, Bindings3}.

%% @private
make_h2o_handler({Module, Opts}) when is_atom(Module) ->
	{Module, Opts};
make_h2o_handler(Term) ->
	erlang:error({badarg, [Term]}).

%% @private
make_h2o_handler(K0, V, Indent, _Level, Acc0, Path, Bindings0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Ref = erlang:make_ref(),
	{V1, Bindings2} = encode_scalar(Ref, 0, [], Bindings1),
	Binding = {h2o_handler, Ref, lists:reverse(Path), V},
	Bindings3 = [Binding | Bindings2],
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	{Acc1, Bindings3}.

%% @private
make_h2o_logger({Module, Opts})
		when is_atom(Module) ->
	{Module, Opts, <<"%h %l %u %t \"%r\" %s %b \"%{Referer}i\" \"%{User-agent}i\"">>, apache};
make_h2o_logger({Module, Opts, Format})
		when is_atom(Module)
		andalso is_binary(Format) ->
	{Module, Opts, Format, apache};
make_h2o_logger({Module, Opts, Format, Escape})
		when is_atom(Module)
		andalso is_binary(Format)
		andalso (Escape == apache orelse Escape == json) ->
	{Module, Opts, Format, Escape};
make_h2o_logger(Term) ->
	erlang:error({badarg, [Term]}).

%% @private
make_h2o_logger(K0, V={_, _, Format, Escape}, Indent, Level, Acc0, Path, Bindings0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Ref = erlang:make_ref(),
	{V1, Bindings2} = encode_dict([
		{<<"reference">>, Ref},
		{<<"format">>, Format},
		{<<"escape">>, atom_to_binary(Escape, latin1)}
	], Level + 1, [], Path, Bindings1),
	Binding = {h2o_logger, Ref, lists:reverse(Path), V},
	Bindings3 = [Binding | Bindings2],
	Acc1 = [Acc0, $\n, Indent, K1, $:, V1],
	{Acc1, Bindings3}.

%% @private
make_h2o_websocket({Module, Opts}) when is_atom(Module) ->
	{Module, Opts};
make_h2o_websocket(Term) ->
	erlang:error({badarg, [Term]}).

%% @private
make_h2o_websocket(K0, V, Indent, _Level, Acc0, Path, Bindings0) ->
	{K1, Bindings1} = encode_scalar(K0, 0, [], Bindings0),
	Ref = erlang:make_ref(),
	{V1, Bindings2} = encode_scalar(Ref, 0, [], Bindings1),
	Binding = {h2o_websocket, Ref, lists:reverse(Path), V},
	Bindings3 = [Binding | Bindings2],
	Acc1 = [Acc0, $\n, Indent, K1, $:, $\s, V1],
	{Acc1, Bindings3}.
