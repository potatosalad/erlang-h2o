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
	id  = undefined :: undefined | h2o_port:id(),
	ref = undefined :: undefined | h2o_port:ref()
}).

-endif.
