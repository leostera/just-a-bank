%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : foreign_api.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(foreign_api).
-include("backend.hrl").

-export([start/0, start_link/0]).
-export([init/1, auth/2, auth/3, info/2, info/3,
         op/2, op/3, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-record(state, {account}).
-define(WARN(Event), error_logger:warning_report(io_lib:format("Unkown Msg in ~p (~p): ~p~n", [?MODULE, ?LINE, Event]))).

start() -> gen_fsm:start(?MODULE, [], [{debug, [trace]}]).

start_link() -> gen_fsm:start_link(?MODULE, [], [{debug, [trace]}]).

init([]) ->
    {ok, auth, #state{}}.

auth(Event, State) ->
    ?WARN(Event),
    {next_state, auth, State}.

auth({BankName, ID}, _From, State) ->
    case lists:member({BankName, ID}, ?VALID_BANKS) of
        true -> {reply, ok, info, State};
        false -> {reply, denied, auth, State}
    end.

info(Event, State) ->
    ?WARN(Event),
    {next_state, info, State}.

info({account, Id}, _From, State) ->
    case backend:account(Id) of
        [{error, instance}] -> {reply, error, info, State};
        [_] -> {reply, ok, op, State#state{account=Id}}
    end.

op(Event, State) ->
    ?WARN(Event),
    {next_state, op, State}.

op({withdraw, Amount}, _From, State = #state{account=No}) ->
    [#account{pin=Pin}] = backend:account(No),
    case backend:withdraw(No, Pin, Amount) of
        ok -> {reply, ok, op, State};
        {error, _Reason} -> {reply, denied, op, State}
    end;
op({deposit, Amount}, _From, State = #state{account=No}) ->
    case backend:deposit(No, Amount) of
        ok -> {reply, ok, op, State};
        {error, _Reason} -> {reply, denied, op, State}
    end;
op(change_account, _From, State = #state{}) ->
    {reply, ok, info, State#state{account=undefined}}.

handle_event(Event, StateName, State) ->
    ?WARN(Event),
    {next_state, StateName, State}.

handle_sync_event(off, _From, _StateName, State) ->
    {stop, normal, ok, State}.

handle_info(Info, StateName, State) ->
    ?WARN(Info),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_Vsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.
