%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : stats_online.erl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(stats_online).

%% Keeps track of how many users are online
-behaviour(gen_event).
-vsn('1.0').

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {num=0, users=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    {ok, #state{}}.

handle_event({login, Id, true}, S = #state{num=N, users=Users}) ->
    case lists:member(Id, Users) of
        true -> {ok, S};
        false -> {ok, S#state{num=N+1, users=[Id|Users]}}
    end;
handle_event({eject, Id}, S = #state{num=N, users=Users}) ->
    case lists:member(Id, Users) of
        true -> {ok, S#state{num=N-1, users = Users -- [Id]}};
        false -> {ok, S}
    end;
handle_event(_, State) ->
    {ok, State}.

handle_call(get_stats, S = #state{num=N, users=Users}) ->
    {ok, [{count, N}, {users, Users}], S}.

handle_info(_Event, State) ->
    {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_Vsn, State, _Extra) -> {ok, State}.
