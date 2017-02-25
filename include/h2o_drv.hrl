%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2015-2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  16 Feb 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------

-ifndef(H2O_DRV_HRL).
-define(H2O_DRV_HRL, 1).

-define(H2O_DRV_ATOM, 'h2o_drv').
-define(H2O_DRV_NAME, "h2o_drv").

-define(H2O_DRV_REP_ERROR,	0).
-define(H2O_DRV_REP_OK,		1).
-define(H2O_DRV_REP,		2).
-define(H2O_DRV_REP_TERM,	131).

% port_control requests
-define(H2O_DRV_REQ_CLIENT,		1).
-define(H2O_DRV_REQ_SERVER,		2).
-define(H2O_DRV_REQ_CLOSE,		3).
-define(H2O_DRV_REQ_SETOPTS,	4).
-define(H2O_DRV_REQ_GETOPTS,	5).
-define(H2O_DRV_REQ_SETCFG,		6).
-define(H2O_DRV_REQ_GETCFG,		7).

-endif.
