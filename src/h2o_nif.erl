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

-export([register_monitor/0]).
-export([monitor_ack/1]).
-export([roundtrip/0]).

%% h2o_nif/filter.c.h
-export([filter_read_start/1]).
-export([filter_read/1]).
-export([filter_event_setup_next_ostream/1]).

%% h2o_nif/handler.c.h
-export([handler_read_start/1]).
-export([handler_read/1]).
-export([handler_event_reply/4]).
-export([handler_event_reply_batch/1]).
-export([handler_event_reply_multi/4]).

%% h2o_nif/logger.c.h
-export([logger_read_start/1]).
-export([logger_read/1]).

%% h2o_nif/port.c.h
-export([port_lookup/1]).
-export([port_kill/1]).
-export([port_open/0]).
-export([port_open/1]).
-export([port_close/1]).
-export([port_connect/2]).
-export([port_info/0]).
-export([port_info/1]).
-export([port_info/2]).
-export([port_is_alive/1]).
-export([port_getopt/2]).
-export([port_setopt/3]).
-export([port_accept/1]).
-export([port_gc/0]).

%% h2o_nif/request.c.h
-export([request_add_header/3]).
-export([request_delegate/1]).
-export([request_info/0]).
-export([request_send_inline/2]).
-export([request_set_status/2]).
-export([request_reply/4]).

%% h2o_nif/server.c.h
-export([server_open/0]).
-export([server_getcfg/1]).
-export([server_setcfg/2]).
-export([server_start/1]).

%% h2o_nif/string.c.h
-export([string_tolower/1]).
-export([string_strtolower/1]).
-export([string_toupper/1]).
-export([string_strtoupper/1]).
-export([string_lcstris/2]).
-export([string_base64_encode_capacity/1]).
-export([string_decode_base64url/1]).
-export([string_base64_encode/2]).
-export([string_hex_decode/1]).
-export([string_hex_encode/1]).
-export([string_uri_escape/2]).
-export([string_get_filext/1]).
-export([string_str_stripws/1]).
-export([string_htmlescape/1]).

% -export([atom_check/1]).
% -export([server_open/0]).
% -export([server_setcfg/2]).
% -export([server_getstatus/1]).
% -export([handler_accept/2]).
% -export([request_reply/1]).
% -export([request_is_websocket_handshake/1]).
% -export([request_upgrade_to_websocket/1]).

-on_load(init/0).

register_monitor() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

monitor_ack(_USec) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

roundtrip() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/filter.c.h
%%%===================================================================

filter_read_start(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

filter_read(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

filter_event_setup_next_ostream(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/handler.c.h
%%%===================================================================

handler_read_start(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

handler_read(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

handler_event_reply(_Port, _Status, _Headers, _Body) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

handler_event_reply_batch(_List) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

handler_event_reply_multi(_Port, _Status, _Headers, _Body) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/logger.c.h
%%%===================================================================

logger_read_start(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

logger_read(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/port.c.h
%%%===================================================================

port_lookup(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_kill(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_open() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_open(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_close(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_connect(_Port, _NewOwner) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_info() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_info(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_info(_Port, _Item) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_is_alive(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_getopt(_Port, _Opt) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_setopt(_Port, _Opt, _Val) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_accept(_Port) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

port_gc() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/request.c.h
%%%===================================================================

request_add_header(_Request, _Name, _Value) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_delegate(_Request) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_info() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_send_inline(_Request, _Body) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_set_status(_Request, _Status) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

request_reply(_Request, _Status, _Headers, _Body) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/server.c.h
%%%===================================================================

server_open() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_getcfg(_Server) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_setcfg(_Server, _Config) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

server_start(_Server) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% h2o_nif/string.c.h
%%%===================================================================

string_tolower(_Ch) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_strtolower(_S) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_toupper(_Ch) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_strtoupper(_S) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_lcstris(_Target, _Test) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_base64_encode_capacity(_Len) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_decode_base64url(_Src) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_base64_encode(_Src, _UrlEncoded) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_hex_decode(_Src) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_hex_encode(_Src) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_uri_escape(_S, _PreserveChars) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_get_filext(_Path) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_str_stripws(_S) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

string_htmlescape(_Src) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

% atom_check(_Atom) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

% server_getstatus(_Server) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

% handler_accept(_Handler, _Timeout) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

% request_reply(_Request) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

% request_is_websocket_handshake(_Request) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

% request_upgrade_to_websocket(_Request) ->
% 	erlang:nif_error({nif_not_loaded, ?MODULE}).

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
