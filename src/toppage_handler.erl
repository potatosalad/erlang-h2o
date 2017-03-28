%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  11 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(toppage_handler).
% -behaviour(h2o_handler).

%% API
% -export([start_link/1]).
% -export([execute/2]).
-export([start_link/2]).
-export([execute/3]).
-export([on_req/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Req, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, execute, [self(), Req, Opts]),
	{ok, Pid}.

execute(Parent, Req, Opts) ->
	% ok = h2o_handler_event:ack(Parent, Event),
	ok = receive
		{shoot, Parent, Req} ->
			ok
	end,
	on_req(Req, Opts).

on_req(Req0, []) ->
	% {ok, Body, Req1} = h2o_req:read_part_body(Req0),
	% io:format("Body: ~p~n", [Body]),
	% {ok, _Headers, Req1} = h2o_req:read_part(Req0),
	% io:format("read_part: ~p~n", [catch h2o_req:read_part(Req1)]),
	% io:format("read_part_body: ~p~n", [catch h2o_req:read_part_body(Req1)]),
	% {ok, Body, Req1} = read_body(Req0, #{length => 512}, []),
	% {ok, Body, Req1} = h2o_req:read_part_body(Req0),
	% {more, Body0, Req1} = h2o_req:read_body(Req0, ),
	% {ok, Body1, Req2} = h2o_req:read_body(Req1, #{length => 512}),
	% io:format("Body0: ~p~n", [Body0]),
	% io:format("Body1: ~p~n", [Body1]),
	% io:format("Req:   ~s~n", [h2o_handler:pretty_print(Req2)]),
	% _Req3 = h2o_req:reply(Req2, 200, #{
	% 	<<"content-type">> => <<"text/plain">>
	% }, <<"Hello world!\n">>),
	% io:format("Req:   ~s~n", [h2o_handler:pretty_print(_Req3)]),
	Req1 = h2o_req:stream_reply(Req0, 200, #{
		<<"content-type">> => <<"text/plain">>
	}),
	ok = h2o_req:stream_body(Req1, nofin, <<"Hello ">>),
	timer:sleep(timer:seconds(5)),
	ok = h2o_req:stream_body(Req1, fin, <<"world!\n">>),
	% ok = h2o_req:stream_body(Req1, fin, <<"Hello world!\n">>),
	% _ = h2o_req:reply(Req0, 200, #{
	% 	<<"content-type">> => <<"text/plain">>
	% }, <<"Hello world!\n">>),
	ok.

% read_body(Req0, Opts, Acc) ->
% 	case h2o_req:read_body(Req0, Opts) of
% 		{more, Body, Req1} ->
% 			read_body(Req1, Opts, [Body | Acc]);
% 		{ok, Body, Req1} ->
% 			{ok, lists:reverse([Body | Acc]), Req1}
% 	end.

% start_link(Opts) ->
% 	Pid = proc_lib:spawn_link(?MODULE, execute, [self(), Opts]),
% 	{ok, Pid}.

% execute(Parent, []) ->
% 	Event = receive
% 		{shoot, Parent, E} ->
% 			E
% 	end,
% 	ok = h2o_handler_event:reply(Event, 200, #{
% 		<<"content-length">> => <<"12">>,
% 		<<"content-type">> => <<"text/plain">>
% 	}, <<"Hello world!">>),
% 	ok.
