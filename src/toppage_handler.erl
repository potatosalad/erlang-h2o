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

%% API
-export([execute/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

execute(Socket, Host, Path, Opts) ->
	% io:format("~p, ~p, ~p, ~p", [Socket, Host, Path, Opts]),
	_ = Host,
	_ = Path,
	_ = Opts,
	ok = h2o_req:reply(Socket, 200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello world!">>),
	ok.
	% exit(normal).
