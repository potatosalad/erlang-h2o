%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  27 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_res).

-include("h2o_batch.hrl").
-include("h2o_req.hrl").

%% Public API
-export([from_struct/1]).
-export([to_struct/1]).
%% Status API
-export([delete_status/1]).
-export([get_status/1]).
-export([set_status/2]).
%% Reason API
-export([delete_reason/1]).
-export([get_reason/1]).
-export([set_reason/2]).
%% Content Length API
-export([delete_content_length/1]).
-export([get_content_length/1]).
-export([set_content_length/2]).
%% Headers API
-export([delete_headers/1]).
-export([get_headers/1]).
-export([set_headers/2]).
%% Header API
-export([delete_header/2]).
-export([get_header/2]).
-export([get_header/3]).
-export([set_header/3]).
%% Response API
-export([response/1]).

%% Types
-type res() :: #h2o_res{}.
-export_type([res/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

from_struct(#{
	'__struct__'   := h2o_res,
	status         := Status,
	reason         := Reason,
	content_length := ContentLength,
	headers        := Headers
}) ->
	#h2o_res{
		status         = Status,
		reason         = Reason,
		content_length = ContentLength,
		headers        = Headers
	}.

to_struct(#h2o_res{
	status         = Status,
	reason         = Reason,
	content_length = ContentLength,
	headers        = Headers
}) ->
	#{
		'__struct__'   => h2o_res,
		status         => Status,
		reason         => Reason,
		content_length => ContentLength,
		headers        => Headers
	}.

%%%===================================================================
%%% Status API
%%%===================================================================

delete_status(Res=#h2o_res{status={OldStatus, _}}) ->
	Res#h2o_res{status=OldStatus};
delete_status(Res=#h2o_res{}) ->
	Res.

get_status(#h2o_res{status={_, Status}}) ->
	Status;
get_status(#h2o_res{status=Status}) ->
	Status.

set_status(Res=#h2o_res{status={OldStatus, _}}, NewStatus) ->
	Res#h2o_res{status={OldStatus, NewStatus}};
set_status(Res=#h2o_res{status=OldStatus}, NewStatus) ->
	Res#h2o_res{status={OldStatus, NewStatus}}.

%%%===================================================================
%%% Reason API
%%%===================================================================

delete_reason(Res=#h2o_res{reason={OldReason, _}}) ->
	Res#h2o_res{reason=OldReason};
delete_reason(Res=#h2o_res{}) ->
	Res.

get_reason(#h2o_res{reason={_, Reason}}) ->
	Reason;
get_reason(#h2o_res{reason=Reason}) ->
	Reason.

set_reason(Res=#h2o_res{reason={OldReason, _}}, NewReason) ->
	Res#h2o_res{reason={OldReason, NewReason}};
set_reason(Res=#h2o_res{reason=OldReason}, NewReason) ->
	Res#h2o_res{reason={OldReason, NewReason}}.

%%%===================================================================
%%% Content Length API
%%%===================================================================

delete_content_length(Res=#h2o_res{content_length={OldContentLength, _}}) ->
	Res#h2o_res{content_length=OldContentLength};
delete_content_length(Res=#h2o_res{}) ->
	Res.

get_content_length(#h2o_res{content_length={_, ContentLength}}) ->
	ContentLength;
get_content_length(#h2o_res{content_length=ContentLength}) ->
	ContentLength.

set_content_length(Res=#h2o_res{content_length={OldContentLength, _}}, NewContentLength) ->
	Res#h2o_res{content_length={OldContentLength, NewContentLength}};
set_content_length(Res=#h2o_res{content_length=OldContentLength}, NewContentLength) ->
	Res#h2o_res{content_length={OldContentLength, NewContentLength}}.

%%%===================================================================
%%% Headers API
%%%===================================================================

delete_headers(Res=#h2o_res{headers={OldHeaders, _}}) ->
	Res#h2o_res{headers=OldHeaders};
delete_headers(Res=#h2o_res{}) ->
	Res.

get_headers(#h2o_res{headers={_, Headers}}) ->
	Headers;
get_headers(#h2o_res{headers=Headers}) ->
	Headers.

set_headers(Res=#h2o_res{headers={OldHeaders, _}}, NewHeaders) ->
	Res#h2o_res{headers={OldHeaders, NewHeaders}};
set_headers(Res=#h2o_res{headers=OldHeaders}, NewHeaders) ->
	Res#h2o_res{headers={OldHeaders, NewHeaders}}.

%%%===================================================================
%%% Header API
%%%===================================================================

delete_header(Res=#h2o_res{headers={_, Headers}}, Name) ->
	NewHeaders = do_delete_header(Headers, Name),
	set_headers(Res, NewHeaders);
delete_header(Res=#h2o_res{headers=Headers}, Name) ->
	NewHeaders = do_delete_header(Headers, Name),
	set_headers(Res, NewHeaders).

get_header(Res=#h2o_res{}, Name) ->
	get_header(Res, Name, undefined).

get_header(#h2o_res{headers={_, Headers}}, Name, Default) ->
	do_get_header(Headers, Name, Default);
get_header(#h2o_res{headers=Headers}, Name, Default) ->
	do_get_header(Headers, Name, Default).

set_header(Res=#h2o_res{headers={_, Headers}}, Name, Value) ->
	NewHeaders = do_set_header(Headers, Name, Value),
	set_headers(Res, NewHeaders);
set_header(Res=#h2o_res{headers=Headers}, Name, Value) ->
	NewHeaders = do_set_header(Headers, Name, Value),
	set_headers(Res, NewHeaders).

%%%===================================================================
%%% Response API
%%%===================================================================

response(Res=#h2o_res{}) ->
	do_response([status, reason, content_length, headers], Res, #h2o_res{}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
do_delete_header(undefined, _Name) ->
	maps:new();
do_delete_header(List, Name) when is_list(List) ->
	do_delete_header(maps:from_list(List), Name);
do_delete_header(Map, Name) when is_map(Map) ->
	maps:remove(Name, Map).

%% @private
do_get_header(undefined, _Name, Default) ->
	Default;
do_get_header(List, Name, Default) when is_list(List) ->
	case lists:keyfind(Name, 1, List) of
		{Name, Value} ->
			Value;
		false ->
			Default
	end;
do_get_header(Map, Name, Default) when is_map(Map) ->
	maps:get(Name, Map, Default).

%% @private
do_set_header(undefined, Name, Value) ->
	maps:put(Name, Value, maps:new());
do_set_header(List, Name, Value) when is_list(List) ->
	do_set_header(maps:from_list(List), Name, Value);
do_set_header(Map, Name, Value) when is_map(Map) ->
	maps:put(Name, Value, Map).

%% @private
do_response([status | T], Res=#h2o_res{status={_, Status}}, Acc) ->
	do_response(T, Res, Acc#h2o_res{status=Status});
do_response([reason | T], Res=#h2o_res{reason={_, Reason}}, Acc) ->
	do_response(T, Res, Acc#h2o_res{reason=Reason});
do_response([content_length | T], Res=#h2o_res{content_length={_, ContentLength}}, Acc) ->
	do_response(T, Res, Acc#h2o_res{content_length=ContentLength});
do_response([headers | T], Res=#h2o_res{headers={_, Headers}}, Acc) ->
	do_response(T, Res, Acc#h2o_res{headers=Headers});
do_response([_ | T], Res, Acc) ->
	do_response(T, Res, Acc);
do_response([], _Res, Acc) ->
	Acc.
