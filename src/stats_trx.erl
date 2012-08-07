%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : stats_trx.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(stats_trx).

%% Keeps track of transactions
-behaviour(gen_event).
-vsn('1.0').

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {in=0, out=0, within=0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    {ok, #state{}}.

handle_event({withdraw, _No, Amount}, S = #state{out=N}) ->
    {ok, S#state{out=N+Amount}};
handle_event({deposit, _No, Amount}, S = #state{in=N}) ->
    {ok, S#state{in=N+Amount}};
handle_event({transfer, _From, _To, Amount}, S = #state{within=N}) ->
    {ok, S#state{within=N+Amount}};
handle_event(_, State) ->
    {ok, State}.

handle_call(get_stats, S = #state{in=I, out=O, within=W}) ->
    {ok, [{in, I}, {out, O}, {within, W}], S}.

handle_info(_Event, State) ->
    {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_Vsn, State, _Extra) -> {ok, State}.
