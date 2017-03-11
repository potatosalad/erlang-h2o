%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  03 Mar 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------

-ifndef(H2O_PORT_HRL).
-define(H2O_PORT_HRL, 1).

-record(h2o_port, {
	id = undefined :: undefined | h2o_port:id()
}).

-define(H2O_NIF_PORT_FLAG_ALC, 16#0001).
-define(H2O_NIF_PORT_FLAG_OPN, 16#0002).
-define(H2O_NIF_PORT_FLAG_CFG, 16#0004).
-define(H2O_NIF_PORT_FLAG_SRT, 16#0008).
-define(H2O_NIF_PORT_FLAG_LST, 16#0010).
-define(H2O_NIF_PORT_FLAG_ACC, 16#0020).
-define(H2O_NIF_PORT_FLAG_FIN, 16#0040).

-define(H2O_NIF_PORT_STATE_CLOSED, 16#0000).
-define(H2O_NIF_PORT_STATE_ALLOCATED, (?H2O_NIF_PORT_FLAG_ALC)).
-define(H2O_NIF_PORT_STATE_OPEN, (?H2O_NIF_PORT_STATE_ALLOCATED bor ?H2O_NIF_PORT_FLAG_OPN)).
-define(H2O_NIF_PORT_STATE_CONFIGURED, (?H2O_NIF_PORT_STATE_OPEN bor ?H2O_NIF_PORT_FLAG_CFG)).
-define(H2O_NIF_PORT_STATE_STARTED, (?H2O_NIF_PORT_STATE_CONFIGURED bor ?H2O_NIF_PORT_FLAG_SRT)).
-define(H2O_NIF_PORT_STATE_LISTENING, (?H2O_NIF_PORT_STATE_OPEN bor ?H2O_NIF_PORT_FLAG_LST)).
-define(H2O_NIF_PORT_STATE_ACCEPTING, (?H2O_NIF_PORT_STATE_OPEN bor ?H2O_NIF_PORT_FLAG_ACC)).
-define(H2O_NIF_PORT_STATE_FINISHED, (?H2O_NIF_PORT_STATE_OPEN bor ?H2O_NIF_PORT_FLAG_FIN)).

-endif.
