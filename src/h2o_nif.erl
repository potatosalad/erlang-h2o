%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  25 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_nif).

-export([server_open/0]).
-export([server_getcfg/1]).
-export([server_setcfg/2]).
-export([server_getstatus/1]).
-export([server_start/1]).
-export([request_reply/1]).

-on_load(init/0).

server_open() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_getcfg(_Server) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_setcfg(_Server, _Config) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_getstatus(_Server) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_start(_Server) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_reply(_Request) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
init() ->
	SoName = filename:join(priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).

%% @private
priv_dir() ->
	case code:priv_dir(h2o) of
		{error, bad_name} ->
			case code:which(h2o) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.
