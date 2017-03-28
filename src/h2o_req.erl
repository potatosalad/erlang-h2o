%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  10 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o_req).

-include("h2o_batch.hrl").
-include("h2o_req.hrl").

%% Public API
-export([from_struct/1]).
-export([to_struct/1]).
%% Request API
-export([header/2]).
-export([header/3]).
-export([headers/1]).
-export([parse_header/2]).
-export([parse_header/3]).
%% Request Body API
-export([has_body/1]).
-export([body_length/1]).
-export([read_body/1]).
-export([read_body/2]).
-export([read_urlencoded_body/1]).
-export([read_urlencoded_body/2]).
%% Request Multipart API
-export([read_part/1]).
-export([read_part/2]).
-export([read_part_body/1]).
-export([read_part_body/2]).
%% Response API
-export([delete_resp_body/1]).
-export([has_resp_body/1]).
-export([set_resp_body/2]).
-export([delete_resp_cookie/2]).
-export([has_resp_cookie/2]).
-export([set_resp_cookie/3]).
-export([set_resp_cookie/4]).
-export([delete_resp_header/2]).
-export([has_resp_header/2]).
-export([set_resp_header/3]).
-export([delete_resp_headers/1]).
-export([get_resp_headers/1]).
-export([get_resp_headers/2]).
-export([has_resp_headers/1]).
-export([set_resp_headers/2]).
-export([reply/2]).
-export([reply/3]).
-export([reply/4]).
-export([stream_reply/2]).
-export([stream_reply/3]).
-export([stream_body/3]).

%% Types
-type req() :: #h2o_req{}.
-export_type([req/0]).

-type resp_body() :: iodata()
	| {sendfile, non_neg_integer(), pos_integer(), file:name_all()}.
-export_type([resp_body/0]).

-type cookie_opts() :: #{
	domain => binary(),
	http_only => boolean(),
	max_age => non_neg_integer(),
	path => binary(),
	secure => boolean()
}.
-export_type([cookie_opts/0]).

% -export([info/0]).
% -export([add_header/3]).
% -export([delegate/1]).
% -export([send_inline/2]).
% -export([set_status/2]).

% %% Response API
% -export([reply/4]).

% %% Macros
% -define(call_close(R, F),
% 	case (F) of
% 		ok ->
% 			h2o_port:close(R);
% 		Error ->
% 			Error
% 	end).

%%%===================================================================
%%% Public API
%%%===================================================================

from_struct(#{
	'__struct__'  := h2o_req,
	event         := Event,
	authority     := Authority,
	body_length   := BodyLength,
	has_body      := HasBody,
	has_read_body := HasReadBody,
	has_sent_resp := HasSentResp,
	headers       := Headers,
	host          := Host,
	method        := Method,
	multipart     := Multipart,
	path          := Path,
	peer          := Peer,
	port          := Port,
	resp_body     := RespBody,
	resp_cookies  := RespCookies,
	resp_headers  := RespHeaders,
	scheme        := Scheme,
	streamid      := StreamID,
	version       := Version
}) ->
	#h2o_req{
		event         = Event,
		authority     = Authority,
		body_length   = BodyLength,
		has_body      = HasBody,
		has_read_body = HasReadBody,
		has_sent_resp = HasSentResp,
		headers       = Headers,
		host          = Host,
		method        = Method,
		multipart     = Multipart,
		path          = Path,
		peer          = Peer,
		port          = Port,
		resp_body     = RespBody,
		resp_cookies  = RespCookies,
		resp_headers  = RespHeaders,
		scheme        = Scheme,
		streamid      = StreamID,
		version       = Version
	}.

to_struct(#h2o_req{
	event         = Event,
	authority     = Authority,
	body_length   = BodyLength,
	has_body      = HasBody,
	has_read_body = HasReadBody,
	has_sent_resp = HasSentResp,
	headers       = Headers,
	host          = Host,
	method        = Method,
	multipart     = Multipart,
	path          = Path,
	peer          = Peer,
	port          = Port,
	resp_body     = RespBody,
	resp_cookies  = RespCookies,
	resp_headers  = RespHeaders,
	scheme        = Scheme,
	streamid      = StreamID,
	version       = Version
}) ->
	#{
		'__struct__'  => h2o_req,
		event         => Event,
		authority     => Authority,
		body_length   => BodyLength,
		has_body      => HasBody,
		has_read_body => HasReadBody,
		has_sent_resp => HasSentResp,
		headers       => Headers,
		host          => Host,
		method        => Method,
		multipart     => Multipart,
		path          => Path,
		peer          => Peer,
		port          => Port,
		resp_body     => RespBody,
		resp_cookies  => RespCookies,
		resp_headers  => RespHeaders,
		scheme        => Scheme,
		streamid      => StreamID,
		version       => Version
	}.

%%%===================================================================
%%% Request API
%%%===================================================================

header(Req, Name) ->
	header(Req, Name, undefined).

header(#h2o_req{headers=Headers}, Name, Default) when is_list(Headers) ->
	case lists:keyfind(Name, 1, Headers) of
		{Name, Value} ->
			Value;
		false ->
			Default
	end;
header(#h2o_req{headers=Headers}, Name, Default) when is_map(Headers) ->
	maps:get(Name, Headers, Default).

headers(#h2o_req{headers=Headers}) ->
	Headers.

parse_header(Req, Name = <<"content-length">>) ->
	parse_header(Req, Name, 0, fun cow_http_hd:parse_content_length/1);
parse_header(Req, Name = <<"cookie">>) ->
	parse_header(Req, Name, [], fun cow_cookie:parse_cookie/1);
%% @todo That header is abstracted out and should never reach cowboy_req.
% parse_header(Name = <<"transfer-encoding">>, Req) ->
% 	parse_header(Name, Req, [<<"identity">>], fun cow_http_hd:parse_transfer_encoding/1);
parse_header(Req, Name) ->
	parse_header(Req, Name, undefined).

parse_header(Req, Name, Default) ->
	parse_header(Req, Name, Default, parse_header_fun(Name)).

%% @private
parse_header_fun(<<"accept">>) -> fun cow_http_hd:parse_accept/1;
parse_header_fun(<<"accept-charset">>) -> fun cow_http_hd:parse_accept_charset/1;
parse_header_fun(<<"accept-encoding">>) -> fun cow_http_hd:parse_accept_encoding/1;
parse_header_fun(<<"accept-language">>) -> fun cow_http_hd:parse_accept_language/1;
parse_header_fun(<<"authorization">>) -> fun cow_http_hd:parse_authorization/1;
parse_header_fun(<<"connection">>) -> fun cow_http_hd:parse_connection/1;
parse_header_fun(<<"content-length">>) -> fun cow_http_hd:parse_content_length/1;
parse_header_fun(<<"content-type">>) -> fun cow_http_hd:parse_content_type/1;
parse_header_fun(<<"cookie">>) -> fun cow_cookie:parse_cookie/1;
parse_header_fun(<<"expect">>) -> fun cow_http_hd:parse_expect/1;
parse_header_fun(<<"if-match">>) -> fun cow_http_hd:parse_if_match/1;
parse_header_fun(<<"if-modified-since">>) -> fun cow_http_hd:parse_if_modified_since/1;
parse_header_fun(<<"if-none-match">>) -> fun cow_http_hd:parse_if_none_match/1;
parse_header_fun(<<"if-unmodified-since">>) -> fun cow_http_hd:parse_if_unmodified_since/1;
parse_header_fun(<<"range">>) -> fun cow_http_hd:parse_range/1;
parse_header_fun(<<"sec-websocket-extensions">>) -> fun cow_http_hd:parse_sec_websocket_extensions/1;
parse_header_fun(<<"sec-websocket-protocol">>) -> fun cow_http_hd:parse_sec_websocket_protocol_req/1;
parse_header_fun(<<"transfer-encoding">>) -> fun cow_http_hd:parse_transfer_encoding/1;
parse_header_fun(<<"upgrade">>) -> fun cow_http_hd:parse_upgrade/1;
parse_header_fun(<<"x-forwarded-for">>) -> fun cow_http_hd:parse_x_forwarded_for/1.

%% @private
parse_header(Req, Name, Default, ParseFun) ->
	case header(Req, Name) of
		undefined -> Default;
		Value -> ParseFun(Value)
	end.

%%%===================================================================
%%% Request Body API
%%%===================================================================

has_body(#h2o_req{has_body=HasBody}) ->
	HasBody.

body_length(#h2o_req{body_length=Length}) ->
	Length.

read_body(Req) ->
	read_body(Req, #{}).

read_body(Req=#h2o_req{has_body=false}, _Opts) ->
	{ok, <<>>, Req};
read_body(Req=#h2o_req{has_read_body=true}, _Opts) ->
	{ok, <<>>, Req};
read_body(Req=#h2o_req{event=Event, event_type=EventType}, Opts) ->
	Length = maps:get(length, Opts, 8000000),
	Period = maps:get(period, Opts, 15000),
	Timeout = maps:get(timeout, Opts, Period + 1000),
	ok = do_read_body(Event, EventType, Length),
	receive
		{entity, Body} ->
			{more, Body, Req};
		{entity, BodyLength, Body} ->
			{ok, Body, set_body_length(Req, BodyLength)}
	after
		Timeout ->
			exit(timeout)
	end.

%% @private
do_read_body(Event, filter, Length) ->
	h2o_batch:cast({?H2O_BATCH_filter_event_read_entity, {Event, self(), Length}}).

%% @private
set_body_length(Req=#h2o_req{headers=Headers}, BodyLength) when is_list(Headers) ->
	Req#h2o_req{
		body_length = BodyLength,
		has_read_body = true,
		headers = [{<<"content-length">>, integer_to_binary(BodyLength)} | Headers]
	};
set_body_length(Req=#h2o_req{headers=Headers}, BodyLength) when is_map(Headers) ->
	Req#h2o_req{
		body_length = BodyLength,
		has_read_body = true,
		headers = Headers#{<<"content-length">> => integer_to_binary(BodyLength)}
	}.

read_urlencoded_body(Req) ->
	read_urlencoded_body(Req, #{length => 64000, period => 5000}).

read_urlencoded_body(Req0, Opts) ->
	{ok, Body, Req1} = read_body(Req0, Opts),
	{ok, cow_qs:parse_qs(Body), Req1}.

%%%===================================================================
%%% Request Multipart API
%%%===================================================================

read_part(Req) ->
	read_part(Req, #{length => 64000, period => 5000}).

read_part(Req=#h2o_req{multipart=undefined}, Opts) ->
	read_part(init_multipart(Req), Opts);
read_part(Req0, Opts) ->
	{Data, Req1} = stream_multipart(Req0, Opts),
	read_part(Data, Opts, Req1).

%% @private
read_part(Buffer0, Opts, Req0=#h2o_req{multipart={Boundary, _}}) ->
	case cow_multipart:parse_headers(Buffer0, Boundary) of
		more ->
			{Data, Req1} = stream_multipart(Req0, Opts),
			read_part(<< Buffer0/binary, Data/binary >>, Opts, Req1);
		{more, Buffer1} ->
			{Data, Req1} = stream_multipart(Req0, Opts),
			read_part(<< Buffer1/binary, Data/binary >>, Opts, Req1);
		{ok, Headers, Rest} ->
			{ok, Headers, Req0#h2o_req{multipart={Boundary, Rest}}};
		{done, _} ->
			{done, Req0#h2o_req{multipart=done}}
	end.

read_part_body(Req) ->
	read_part_body(Req, #{}).

read_part_body(Req=#h2o_req{multipart=undefined}, Opts) ->
	read_part_body(init_multipart(Req), Opts);
read_part_body(Req, Opts) ->
	read_part_body(<<>>, Opts, Req, <<>>).

%% @private
read_part_body(Buffer, Opts, Req0=#h2o_req{multipart={Boundary, _}}, Acc) ->
	Length = maps:get(length, Opts, 8000000),
	case byte_size(Acc) > Length of
		true ->
			{more, Acc, Req0#h2o_req{multipart={Boundary, Buffer}}};
		false ->
			{Data, Req1} = stream_multipart(Req0, Opts),
			case cow_multipart:parse_body(<< Buffer/binary, Data/binary >>, Boundary) of
				{ok, Body} ->
					read_part_body(<<>>, Opts, Req1, << Acc/binary, Body/binary >>);
				{ok, Body, Rest} ->
					read_part_body(Rest, Opts, Req1, << Acc/binary, Body/binary >>);
				done ->
					{ok, Acc, Req1};
				{done, Body} ->
					{ok, << Acc/binary, Body/binary >>, Req1};
				{done, Body, Rest} ->
					{ok, << Acc/binary, Body/binary >>, Req1#h2o_req{multipart={Boundary, Rest}}}
			end
	end.

%% @private
init_multipart(Req) ->
	{<<"multipart">>, _, Params} = parse_header(Req, <<"content-type">>),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	Req#h2o_req{multipart={Boundary, <<>>}}.

%% @private
stream_multipart(Req=#h2o_req{multipart=done}, _) ->
	{<<>>, Req};
stream_multipart(Req0=#h2o_req{multipart={_, <<>>}}, Opts) ->
	{_, Data, Req1} = read_body(Req0, Opts),
	{Data, Req1};
stream_multipart(Req=#h2o_req{multipart={Boundary, Buffer}}, _) ->
	{Buffer, Req#h2o_req{multipart={Boundary, <<>>}}}.

%%%===================================================================
%%% Response API
%%%===================================================================

-spec delete_resp_body(Req)
	-> Req when Req::req().
delete_resp_body(Req=#h2o_req{resp_body=undefined}) ->
	Req;
delete_resp_body(Req=#h2o_req{}) ->
	Req#h2o_req{resp_body=undefined}.

-spec has_resp_body(req())
	-> boolean().
has_resp_body(#h2o_req{resp_body=undefined}) ->
	false;
has_resp_body(#h2o_req{}) ->
	true.

-spec set_resp_body(resp_body(), Req)
	-> Req when Req::req().
set_resp_body(Req, Body) ->
	Req#h2o_req{resp_body=Body}.

-spec delete_resp_cookie(Req, binary())
	-> Req when Req::req().
delete_resp_cookie(Req=#h2o_req{resp_cookies=undefined}, _Name) ->
	Req;
delete_resp_cookie(Req=#h2o_req{resp_cookies=RespCookies0}, Name) ->
	RespCookies1 = maps:remove(Name, RespCookies0),
	case map_size(RespCookies1) of
		0 ->
			Req#h2o_req{resp_cookies=undefined};
		_ ->
			Req#h2o_req{resp_cookies=RespCookies1}
	end.

-spec has_resp_cookie(req(), binary())
	-> boolean().
has_resp_cookie(#h2o_req{resp_cookies=undefined}, _Name) ->
	false;
has_resp_cookie(#h2o_req{resp_cookies=RespCookies}, Name) ->
	maps:is_key(Name, RespCookies).

-spec set_resp_cookie(Req, binary(), iodata())
	-> Req when Req::req().
set_resp_cookie(Req, Name, Value) ->
	set_resp_cookie(Req, Name, Value, #{}).

%% The cookie name cannot contain any of the following characters:
%%   =,;\s\t\r\n\013\014
%%
%% The cookie value cannot contain any of the following characters:
%%   ,; \t\r\n\013\014
-spec set_resp_cookie(Req, binary(), iodata(), cookie_opts())
	-> Req when Req::req().
set_resp_cookie(Req=#h2o_req{resp_cookies=RespCookies}, Name, Value, Opts) when is_map(RespCookies) ->
	Cookie = cow_cookie:setcookie(Name, Value, maps:to_list(Opts)),
	Req#h2o_req{resp_cookies=RespCookies#{Name => Cookie}};
set_resp_cookie(Req=#h2o_req{resp_cookies=undefined}, Name, Value, Opts) ->
	set_resp_cookie(Req#h2o_req{resp_cookies=#{}}, Name, Value, Opts).

-spec delete_resp_header(Req, binary())
	-> Req when Req::req().
delete_resp_header(Req=#h2o_req{resp_headers=undefined}, _Name) ->
	Req;
delete_resp_header(Req=#h2o_req{resp_headers=RespHeaders}, Name) when is_list(RespHeaders) ->
	delete_resp_header(Req#h2o_req{resp_headers=maps:from_list(RespHeaders)}, Name);
delete_resp_header(Req=#h2o_req{resp_headers=RespHeaders}, Name) when is_map(RespHeaders) ->
	Req#h2o_req{resp_headers=maps:remove(Name, RespHeaders)}.

-spec has_resp_header(req(), binary())
	-> boolean().
has_resp_header(#h2o_req{resp_headers=undefined}, _Name) ->
	false;
has_resp_header(#h2o_req{resp_headers=RespHeaders}, Name) when is_list(RespHeaders) ->
	case lists:keyfind(Name, 1, RespHeaders) of
		{Name, _} ->
			true;
		false ->
			false
	end;
has_resp_header(#h2o_req{resp_headers=RespHeaders}, Name) when is_map(RespHeaders) ->
	maps:is_key(Name, RespHeaders).

-spec set_resp_header(Req, binary(), iodata())
	-> Req when Req::req().
set_resp_header(Req=#h2o_req{resp_headers=undefined}, Name, Value) ->
	Req#h2o_req{resp_headers=#{Name => Value}};
set_resp_header(Req=#h2o_req{resp_headers=RespHeaders}, Name, Value) when is_list(RespHeaders) ->
	set_resp_header(Req#h2o_req{resp_headers=maps:from_list(RespHeaders)}, Name, Value);
set_resp_header(Req=#h2o_req{resp_headers=RespHeaders}, Name, Value) when is_map(RespHeaders) ->
	Req#h2o_req{resp_headers=RespHeaders#{Name => Value}}.

-spec delete_resp_headers(Req)
	-> Req when Req::req().
delete_resp_headers(Req=#h2o_req{resp_headers=undefined}) ->
	Req;
delete_resp_headers(Req=#h2o_req{}) ->
	Req#h2o_req{resp_headers=undefined}.

-spec has_resp_headers(req())
	-> boolean().
has_resp_headers(#h2o_req{resp_headers=undefined}) ->
	false;
has_resp_headers(#h2o_req{resp_headers=RespHeaders}) when is_list(RespHeaders) orelse is_map(RespHeaders) ->
	true.

-spec get_resp_headers(req())
	-> h2o:http_headers().
get_resp_headers(Req) ->
	get_resp_headers(Req, #{}).

-spec get_resp_headers(req(), h2o:http_headers())
	-> h2o:http_headers().
% get_resp_headers(#h2o_req{resp_cookies=undefined, resp_headers=undefined}, Headers) when is_list(Headers) ->
% 	maps:from_list(Headers);
% get_resp_headers(#h2o_req{resp_cookies=undefined, resp_headers=undefined}, Headers) when is_map(Headers) ->
% 	Headers;
get_resp_headers(#h2o_req{resp_cookies=RespCookies, resp_headers=RespHeaders}, []) ->
	do_resp_headers(RespCookies, RespHeaders);
get_resp_headers(Req=#h2o_req{}, Headers) when is_list(Headers) ->
	get_resp_headers(Req, maps:from_list(Headers));
get_resp_headers(#h2o_req{resp_cookies=RespCookies, resp_headers=RespHeaders}, Headers) when is_map(Headers) andalso map_size(Headers) == 0 ->
	do_resp_headers(RespCookies, RespHeaders);
get_resp_headers(#h2o_req{resp_cookies=RespCookies, resp_headers=RespHeaders}, Headers) ->
	maps:merge(do_resp_headers(RespCookies, RespHeaders), Headers).

%% @private
do_resp_headers(undefined, undefined) ->
	#{};
do_resp_headers(undefined, RespHeaders) when is_list(RespHeaders) ->
	maps:from_list(RespHeaders);
do_resp_headers(undefined, RespHeaders) when is_map(RespHeaders) ->
	RespHeaders;
do_resp_headers(RespCookies, undefined) ->
	#{ <<"set-cookie">> => maps:values(RespCookies) };
do_resp_headers(RespCookies, RespHeaders) when is_list(RespHeaders) ->
	do_resp_headers(RespCookies, maps:from_list(RespHeaders));
do_resp_headers(RespCookies, RespHeaders) ->
	maps:merge(RespHeaders, do_resp_headers(RespCookies, undefined)).

-spec set_resp_headers(Req, h2o:http_headers())
	-> Req when Req::req().
set_resp_headers(Req=#h2o_req{resp_headers=RespHeaders}, Headers) when is_map(RespHeaders) andalso is_map(Headers) ->
	Req#h2o_req{resp_headers=maps:merge(RespHeaders, Headers)};
set_resp_headers(Req=#h2o_req{resp_headers=undefined}, Headers) ->
	Req#h2o_req{resp_headers=Headers};
set_resp_headers(Req=#h2o_req{resp_headers=RespHeaders}, Headers) when is_list(RespHeaders) ->
	set_resp_headers(Req#h2o_req{resp_headers=maps:from_list(RespHeaders)}, Headers);
set_resp_headers(Req, Headers) when is_list(Headers) ->
	set_resp_headers(Req, maps:from_list(Headers)).

-spec reply(Req, h2o:http_status())
	-> Req when Req::req().
reply(Req, Status) ->
	reply(Req, Status, #{}).

-spec reply(Req, h2o:http_status(), h2o:http_headers())
	-> Req when Req::req().
reply(Req=#h2o_req{resp_body=undefined}, Status, Headers) ->
	reply(Req, Status, Headers, <<>>);
reply(Req=#h2o_req{resp_body=Body}, Status, Headers) ->
	reply(Req, Status, Headers, Body).

-spec reply(Req, h2o:http_status(), h2o:http_headers(), resp_body())
	-> Req when Req::req().
reply(#h2o_req{has_sent_resp=V}, _, _, _) when V =/= false ->
	erlang:error(function_clause);
reply(Req, Status, Headers, Body) when is_list(Headers) ->
	reply(Req, Status, maps:from_list(Headers), Body);
reply(Req, Status, Headers=#{<<"content-length">> := _}, Body) ->
	do_reply(Req, Status, Headers, Body);
reply(Req, Status, Headers, Body) ->
	do_reply(Req, Status, Headers#{
		<<"content-length">> => integer_to_binary(iolist_size(Body))
	}, Body).

%% @private
do_reply(Req=#h2o_req{event=Event}, Status, Headers, Body) ->
	RespHeaders = get_resp_headers(Req, Headers),
	ok = h2o_batch:cast({handler_event_reply, Event, Status, RespHeaders, Body}),
	Req#h2o_req{
		has_sent_resp = true,
		resp_body = undefined,
		resp_cookies = undefined,
		resp_headers = undefined
	}.

-spec stream_reply(Req, h2o:http_status())
	-> Req when Req::req().
stream_reply(Req, Status) ->
	stream_reply(Req, Status, #{}).

-spec stream_reply(Req, h2o:http_status(), h2o:http_headers())
	-> Req when Req::req().
stream_reply(#h2o_req{has_sent_resp=V}, _, _) when V =/= false ->
	erlang:error(function_clause);
stream_reply(Req, Status, Headers) ->
	do_stream_reply(Req, Status, Headers).

%% @private
do_stream_reply(Req=#h2o_req{event=Event}, Status, Headers) ->
	RespHeaders = get_resp_headers(Req, Headers),
	ok = h2o_batch:cast({handler_event_stream_reply, Event, Status, RespHeaders}),
	Req#h2o_req{
		has_sent_resp = headers,
		resp_body = undefined,
		resp_cookies = undefined,
		resp_headers = undefined
	}.

-spec stream_body(req(), fin | nofin, iodata())
	-> ok.
stream_body(#h2o_req{has_sent_resp=headers, method = <<"HEAD">>}, _, _) ->
	ok;
stream_body(#h2o_req{has_sent_resp=headers, event=Event}, IsFin=nofin, Data) ->
	case iolist_size(Data) of
		0 ->
			ok;
		_ ->
			h2o_batch:cast({handler_event_stream_body, Event, IsFin, Data})
	end;
stream_body(#h2o_req{has_sent_resp=headers, event=Event}, IsFin, Data) ->
	h2o_batch:cast({handler_event_stream_body, Event, IsFin, Data}).

% info() ->
% 	[begin
% 		{Key, [begin
% 			{Type, {Sec, (NSec div 1000) + (NSec rem 1000)}}
% 		end || {Type, {Sec, NSec}} <- Stat]}
% 	end || {Key, Stat} <- h2o_nif:request_info()].

% add_header(Request, Name, Value) ->
% 	h2o_nif:request_add_header(Request, Name, Value).

% delegate(Request) ->
% 	?call_close(Request,
% 		h2o_nif:request_delegate(Request)).

% send_inline(Request, Body) ->
% 	case h2o_nif:request_send_inline(Request, Body) of
% 		ok ->
% 			h2o_port:close(Request);
% 			% ok;
% 		Error ->
% 			Error
% 	end.

% set_status(Request, Status) ->
% 	h2o_nif:request_set_status(Request, Status).

%%%===================================================================
%%% Public API
%%%===================================================================

% reply(Request, Status, Headers, Body) ->
% 	?call_close(Request,
% 		h2o_nif:request_reply(Request, Status, Headers#{
% 			<<"content-length">> => integer_to_binary(iolist_size(Body))
% 		}, Body)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
