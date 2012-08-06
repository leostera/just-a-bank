%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : thieves.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(thieves).
-vsn('1.0').   
-behaviour(gen_event).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
% Gen Event part
%
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-record(state, {stream}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec () -> 
%
init(_Arg) ->
  {ok, Stream} = file:open("priv/logs/EbankLog.txt", [write]),
  {ok, #state{stream = Stream}}.


%%%%%
%% @spec () -> 
%
handle_event({set_alarm, Alarm}, State) ->
  io:format(State#state.stream, "Set ~p~n", [Alarm]),
  {ok, State};
handle_event({clear_alarm, Alarm}, State) ->
  io:format(State#state.stream, "Clear ~p~n", [Alarm]),
  {ok, State}.

%%%%%
%% @spec () -> 
%
handle_call(_Query, State) -> {ok, undefined, State}.

%%%%%
%% @spec () -> 
%
handle_info(_Info, State) -> {ok, State}.


%%%%%
%% @spec () -> 
%
terminate(swap, State) -> {alarm_handler, ok};
terminate(_Arg, State) -> file:close(State#state.stream).
