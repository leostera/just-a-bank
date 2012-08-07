%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : stats.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a wrapper module to encapsulate calls to gen_event behaviours
%% in a cleaner manner.
-module(stats).
-vsn('1.0').
-define(EVMGR, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0, stop/0, log/1, add_handler/2, delete_handler/2,
         get_stats/1]).

start_link() ->
    gen_event:start_link(?EVMGR).

stop() ->
    gen_event:stop(?EVMGR).

log(Item) ->
    gen_event:notify(?EVMGR, Item).

add_handler(Handler, Args) ->
    gen_event:add_handler(?EVMGR, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?EVMGR, Handler, Args).

get_stats(Handler) ->
    gen_event:call(?EVMGR, Handler, get_stats).
