%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : stats_failed.erl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(stats_failed).

%% Keeps track of how many users failed to log in
-behaviour(gen_event).
-vsn('1.0').

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {failed=0, total=0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    {ok, #state{}}.

handle_event({login, _, true}, S = #state{total=N}) ->
    {ok, S#state{total=N+1}};
handle_event({login, _, false}, S = #state{failed=F, total=N}) ->
    {ok, S#state{failed=F+1, total=N+1}};
handle_event(_, State) ->
    {ok, State}.

handle_call(get_stats, S = #state{failed=F, total=N}) ->
    {ok, [{failed, F}, {total, N}, {pct, (F/N)*100}], S}.

handle_info(_Event, State) ->
    {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_Vsn, State, _Extra) -> {ok, State}.
