%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  24 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(h2o).

-export([flush_handler/1]).
-export([flush_logger/1]).
-export([reductions/3]).
-export([start/0]).
-export([roundtrip/0]).
-export([example/0]).
% -export([server_open/0]).
% -export([server_getcfg/1]).
% -export([server_setcfg/2]).
% -export([server_getstatus/1]).
% -export([server_start/1]).
% -export([handler_accept/1]).
% -export([handler_accept/2]).
% -export([request_reply/1]).
% -export([get_handler/1]).

flush_handler(Port) ->
	Status = 200,
	Headers = #{
		<<"content-length">> => <<"12">>,
		<<"content-type">> => <<"text/plain">>
	},
	Body = <<"Hello world!">>,
	flush_handler(Port, Status, Headers, 0, Body).

%% @private
flush_handler(Port, Status, Headers, N, Body) ->
	receive
		{h2o_port_data, Port, ready_input} ->
			% ok = h2o_nif:handler_event_reply_multi(h2o_nif:handler_read(Port), Status, Headers, Body),
			% flush_handler(Port, Status, Headers, Body)
			% ok = flush_out(),
			flush_handler(h2o_nif:handler_read(Port), Port, Status, Headers, Body, N, [])
			% _ = spawn(fun() ->
				% flush_handler(h2o_nif:handler_read(Port), Port, Status, Headers, Body)
			% end),
			% flush_handler(Port, Status, Headers, Body)
		% ready_input ->
			% flush_handler(h2o_nif:handler_read(Port), Port, Status, Headers, Body)
	end.

% %% @private
% flush_out() ->
% 	receive
% 		Message ->
% 			io:format("Shell got ~p~n", [Message]),
% 			flush_out()
% 	after
% 		0 ->
% 			ok
% 	end.

%% @private
flush_handler([Event | Events], Port, Status, Headers, Body, N, Acc) ->
	NewBody = [Body, integer_to_binary(N)],
	% NewBody = << Body/binary, (integer_to_binary(N))/binary >>,
	% NewHeaders = #{
	% 	<<"content-length">> => integer_to_binary(iolist_size(NewBody)),
	% 	<<"content-type">> => <<"text/plain">>
	% },
	NewHeaders = Headers#{<<"content-length">> => integer_to_binary(iolist_size(NewBody))},
	flush_handler(Events, Port, Status, Headers, Body, N + 1, [{Event, Status, NewHeaders, NewBody} | Acc]);
flush_handler([], Port, Status, Headers, Body, N, Acc) ->
	ok = h2o_nif:handler_event_reply_batch(Acc),
	flush_handler(Port, Status, Headers, N, Body).

% %% @private
% flush_handler([Event | Events], Port, Status, Headers, Body, N, Acc) ->
% 	flush_handler(Events, Port, Status, Headers, Body, N + 1, [{Event, Status, Headers, Body} | Acc]);
% flush_handler([], Port, Status, Headers, Body, _N, Acc) ->
% 	ok = h2o_nif:handler_event_reply_batch(Acc),
% 	flush_handler(Port, Status, Headers, Body).

% %% @private
% flush_handler([Event | Events], Port, Status, Headers, Body) ->
% 	ok = h2o_nif:handler_event_reply(Event, Status, Headers, Body),
% 	flush_handler(Events, Port, Status, Headers, Body);
% % flush_handler([], _Port, _Status, _Headers, _Body) ->
% 	% ok.
% flush_handler([], Port, Status, Headers, Body) ->
% 	flush_handler(Port, Status, Headers, Body).

flush_logger(Port) ->
	% ok = h2o_nif:logger_read_start(Port),
	receive
		{h2o_port_data, Port, ready_input} ->
			io:format("~p~n", [h2o:reductions(h2o_nif, logger_read, [Port])]),
			flush_logger(Port)
	end.

reductions(Module, Function, Arguments) ->
	Parent = self(),
	Pid = spawn(fun() ->
		Self = self(),
		Start = os:timestamp(),
		R0 = process_info(Self, reductions),
		Res = erlang:apply(Module, Function, Arguments),
		R1 = process_info(Self, reductions),
		T = timer:now_diff(os:timestamp(), Start),
		Parent ! {Self, {T, length(Res), R0, R1}}
	end),
	receive
		{Pid, Result} ->
			Result
	end.

start() ->
	application:ensure_all_started(?MODULE).

roundtrip() ->
	{true, USec} = h2o_nif:roundtrip(),
	h2o_nif:monitor_ack(USec).

example() ->
	Config = [
		{<<"listen">>, 8080},
		{<<"num-threads">>, 1},
		% {<<"erlang.logger">>, {toppage_logger, []}},%, <<"%h\1%l\1%u\1%t\1%r\1%s\1%b\1%{Referer}i\1%{User-agent}i\1">>, apache}},
		% {<<"erlang.filter">>, {toppage_filter, []}},
		{<<"hosts">>, [
			{<<"*">>, [
				{<<"paths">>, [
					{<<"/">>, [
						{<<"erlang.handler">>, {toppage_handler, []}}
						% {<<"file.dir">>, <<"/Users/andrew/Documents">>}
					]}
				]}
			]}
		]}
	],
	{ok, Server} = h2o_server:open(),
	{ok, Bindings} = h2o_server:setcfg(Server, Config),
	ok = h2o_server:start(Server),
	{ok, Server, Bindings}.

	% Config = [
	% 	{<<"listen">>, 8080},
	% 	% {<<"max-connections">>, 4096},
	% 	{<<"num-threads">>, 1},
	% 	{<<"hosts">>, [
	% 		{<<"*">>, [
	% 			{<<"paths">>, [
	% 				{<<"/">>, [
	% 					% {<<"fake.handler">>, 0}
	% 					{<<"erlang.handler">>, {toppage_handler, [], worker, 10}}%,
	% 					% {<<"erlang.logger">>, {toppage_logger, [], worker, 10}}
	% 				]}
	% 			]}
	% 		]}
	% 	]}
	% ],
	% supervisor:start_child(h2o_sup, {example_server,
	% 	{h2o_server_sup, start_link, [example, Config]},
	% 	permanent, 5000, supervisor, [h2o_server_sup]}).
	% {ok, SPort} = h2o_server:open(),
	% {ok, [{_, _, _, _, LPort}]} = h2o_server:setcfg(SPort, [
	% % {ok, []} = h2o_server:setcfg(SPort, [
	% 	{<<"listen">>, 8080},
	% 	% {<<"access-log">>, <<"./access.log">>},
	% 	% {<<"error-log">>, <<"./error.log">>},
	% 	{<<"num-threads">>, 1},
	% 	{<<"hosts">>, [
	% 		{<<"*">>, [
	% 			{<<"paths">>, [
	% 				{<<"/">>, [
	% 					% {<<"fake.handler">>, 1}
	% 					{<<"erlang.handler">>, {my_module, []}}%,
	% 					% {<<"file.dir">>, <<"/Users/andrew/Documents">>}
	% 				]}
	% 			]}
	% 		]}
	% 	]}
	% ]),
	% ok = h2o_server:start(SPort),
	% % ok.
	% Loop = fun Loop() ->
	% 	{ok, RPort} = h2o_port:accept(LPort),
	% 	ok = h2o_req:reply(RPort, 200, #{<<"content-type">> => <<"text/plain">>}, <<"Plain Text">>),
	% 	Loop()
	% end,
	% {ok, spawn(Loop)}.
	% _ = application:ensure_all_started(ranch),
	% ranch:start_listener(my_ref, 100, ranch_h2o, [{socket, LPort}], h2o_protocol, []).
	% {ok, LPort}.
	% Loop = fun Loop() ->
	% 	{ok, RPort} = h2o_port:accept(LPort),
	% 	ok = h2o_req:set_status(RPort, 404),
	% 	ok = h2o_req:add_header(RPort, <<"content-type">>, <<"text/plain">>),
	% 	ok = h2o_req:send_inline(RPort, <<"Unknown Error">>),
	% 	Loop()
	% end,
	% {ok, spawn(Loop)}.
	% h2o_port:accept(LPort).
	% {ok, SPort} = h2o_server:open(),
	% {ok, [{_, _, _, _, LPort}]} = h2o_server:setcfg(SPort, [
	% 	{<<"listen">>, 8080},
	% 	{<<"access-log">>, <<"./access.log">>},
	% 	{<<"error-log">>, <<"./error.log">>},
	% 	{<<"num-threads">>, 1},
	% 	{<<"hosts">>, [
	% 		{<<"*">>, [
	% 			{<<"paths">>, [
	% 				{<<"/">>, [
	% 					{<<"erlang.handler">>, {my_module, []}},
	% 					{<<"file.dir">>, <<"/Users/andrew/Documents">>}
	% 				]}
	% 			]}
	% 		]}
	% 	]}
	% ]),
	% ok = h2o_server:start(SPort),
	% Loop = fun Loop() ->
	% 	{ok, RPort} = h2o_port:accept(LPort),
	% 	spawn(fun() ->
	% 		ok = h2o_req:reply(RPort, 200, #{<<"content-type">> => <<"text/plain">>}, <<"Plain Text">>),
	% 		exit(normal)
	% 	end),
	% 	Loop()
	% end,
	% {ok, [begin
	% 	_ = I, spawn(Loop)
	% end || I <- lists:seq(1, 100)]}.

% server_open() ->
% 	h2o_nif:server_open().

% server_getcfg(Server) ->
% 	h2o_nif:server_getcfg(Server).

% server_setcfg(Server, Config) ->
% 	h2o_nif:server_setcfg(Server, h2o_yaml:encode(Config)).

% server_getstatus(Server) ->
% 	h2o_nif:server_getstatus(Server).

% server_start(Server) ->
% 	h2o_nif:server_start(Server).

% handler_accept(Handler) ->
% 	handler_accept(Handler, infinity).

% handler_accept(Handler, Timeout) ->
% 	case h2o_nif:handler_accept(Handler, Timeout) of
% 		{AsyncID} ->
% 			io:format("async request~n"),
% 			receive
% 				{AsyncID, Request} ->
% 					{ok, Request}
% 			end;
% 		Request ->
% 			io:format("sync request~n"),
% 			{ok, Request}
% 	end.

% request_reply(Tag) ->
% 	receive
% 		{h2o_request, Tag, Request} ->
% 			io:format("got request~n"),
% 			h2o_nif:request_reply(Request)
% 	end.

% get_handler(Tag) ->
% 	receive
% 		{h2o_handler, Tag, Handler} ->
% 			{ok, Handler}
% 	end.

% receive_handlers(Server, Acc) ->
% 	receive
% 		{h2o_handler, Server, Handler} ->
% 			receive_handlers(Server, [{}])
