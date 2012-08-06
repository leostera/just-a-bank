%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : atm.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(atm).
-behaviour(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
% Interface
%
-export([start/1, start_link/1, stop/1, card_inserted/2, event/2]).

%%%%%
% gen_fsm part
%
-export([init/1, idle/2, get_pin/2, selection/2, withdraw/2, timeout/2,
         handle_event/3, idle/3, get_pin/3, selection/3, withdraw/3, timeout/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4 ]).

-define(TIME_LIMIT, 10000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {name, accountNo, input = [], pin}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec start(atom()) -> {atom(ok), pid()} | {error, Reason::term()}
%
start(Name) -> gen_fsm:start({global, {?MODULE, Name}}, ?MODULE, Name, []).

%%%%%
%% @spec start_link(atom()) -> {atom(ok), pid()} | {error, Reason::term()}
%
start_link(Name) ->
    gen_fsm:start_link({global, {?MODULE, Name}}, ?MODULE, Name, []).

%%%%%
%% @spec stop(atom()) -> atom(ok)
%
stop(Name) -> gen_fsm:send_event({global, {?MODULE, Name}}, stop).

%%%%%
%% @spec event(atom(), event()) -> atom(ok)
%
event(Name, E) -> gen_fsm:send_event({global, {?MODULE, Name}}, E).

%%%%%
%% @spec event(atom(), backend:accountNo()) -> atom(ok)
%
card_inserted(Name, Account) -> event(Name, {card_inserted, Account}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS/GEN_FSM CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec init(atom()) -> {atom(ok), stateName(), state()} 
%
init(Name) ->
    {ok, idle, #state{name = Name}}.

%%%%%
%% @spec idle(event(), state()) -> {atom(next_state), stateName(), state()} |
%%                                 {atom(stop), atom(normal())}
%
idle({card_inserted, AccountNumber}, #state{name = Name}) ->
    webatm:do(Name, [webatm:display("Please type your PIN code")]),
    {next_state, get_pin, #state{name = Name, accountNo = AccountNumber}, ?TIME_LIMIT};
idle(clear, State) -> clear(idle, State);
idle(cancel, State) -> cancel(State);
idle({digit, _}, State) -> {next_state, idle, State};
idle({selection, _}, State) -> {next_state, idle, State};
idle(enter, State) -> {next_state, idle, State};
idle(timeout, State) -> {next_state, idle, State};
idle(stop, State) -> {stop, normal, State}.

%%%%%
%% @spec get_pin(event(), state()) -> {atom(next_state), stateName(), state()}|
%%                                    {atom(stop), atom(normal())}
%
get_pin(clear, State) -> clear(get_pin, State);
get_pin(cancel, State) -> cancel(State);
get_pin({digit, Digit}, State = #state{name=Name}) ->
    Digits = State#state.input ++ Digit,
    webatm:do(Name, [webatm:display(Digits)]),
    {next_state, get_pin, State#state{input = Digits}, ?TIME_LIMIT};
get_pin(enter, State = #state{name = Name, accountNo = AccountNo, input=Input}) ->
    case backend:pin_valid(AccountNo, Input) of
        true ->
            webatm:do(Name, [webatm:display("Please make your selection")]),
            {next_state, selection, State#state{pin = Input, input = []}, ?TIME_LIMIT};
        false ->
            webatm:do(Name, [
                webatm:display("PIN code incorrect!"),
                webatm:append_line("Please try again.")
            ]),
            {next_state, get_pin, State#state{input = []}, ?TIME_LIMIT}
    end;
get_pin({selection, _}, State) -> {next_state, get_pin, State, ?TIME_LIMIT};
get_pin({card_inserted, _}, State) -> {next_state, get_pin, State, ?TIME_LIMIT};
get_pin(timeout, State) -> {next_state, timeout, State};
get_pin(stop, State) -> {stop, normal, State}.

%%%%%
%% @spec selection(event(), state()) -> 
%%         {atom(next_state), stateName(), state()} |
%%         {atom(stop), atom(normal())}
%
selection(clear, State) -> clear(selection, State);
selection(cancel, State) -> cancel(State);
selection({selection, withdraw}, State = #state{name=Name}) ->
    webatm:do(Name, [
        webatm:high_light("withdraw"),
        webatm:display("How much would you like to withdraw?")
    ]),
    {next_state, withdraw, State, ?TIME_LIMIT};
selection({selection, balance}, State = #state{name=Name}) ->
    webatm:do(Name, [
        webatm:high_light("balance"),
        balance(State)
    ]),
    {next_state, selection, State, ?TIME_LIMIT};
selection({selection, statement}, State = #state{name=Name}) ->
    webatm:do(Name, [
        webatm:high_light("statement"),
        mini_statement(State)
    ]),
    {next_state, selection, State, ?TIME_LIMIT};
selection({digit, _}, State) -> {next_state, selection, State, ?TIME_LIMIT};
selection(enter, State) -> {next_state, selection, State, ?TIME_LIMIT};
selection({card_inserted, _}, State) -> {next_state, selection, State, ?TIME_LIMIT};
selection(timeout, State) -> {next_state, timeout, State};
selection(stop, State) -> {stop, normal, State}.

%%%%%
%% @spec withdraw(event(), state()) -> 
%%         {atom(next_state), stateName(), state()} |
%%         {atom(stop), atom(normal())}
%
withdraw(clear, State) -> clear(withdraw, State);
withdraw(cancel, State) -> cancel(State);
withdraw({digit, Digit}, State = #state{name=Name}) ->
    Input = State#state.input ++ Digit,
    webatm:do(Name, [webatm:display(Input)]),
    {next_state, withdraw, State#state{input = Input}, ?TIME_LIMIT};
withdraw(enter, #state{name=Name, accountNo=AccNo, pin=Pin, input=Input}) ->
    case backend:withdraw(AccNo, Pin, list_to_integer(Input)) of
        ok ->
            webatm:do(Name, [
                webatm:display("Take the money and run."),
                webatm:wait(3500),
                webatm:high_light("off"),
                webatm:eject()
            ]),
            backend:eject(AccNo),
            timer:sleep(3500);
        {error, Reason} ->
            io:format("fail: ~p~n", [Reason]),
            webatm:do(Name, [
                webatm:display("Could not withdraw money!"),
                webatm:append_line(io_lib:format("~p",[Reason])),
                webatm:wait(3500),
                webatm:high_light("off"),
                webatm:eject()
            ]),
            backend:eject(AccNo),
            timer:sleep(3500)
        end,
        {next_state, idle, #state{name = Name}};
withdraw({selection, _}, State) -> {next_state, withdraw, State, ?TIME_LIMIT};
withdraw({card_inserted, _}, State) -> {next_state, withdraw, State, ?TIME_LIMIT};
withdraw(timeout, State) -> {next_state, timeout, State};
withdraw(stop, State) -> {stop, normal, State}.

timeout(_Event, #state{name = Name, accountNo = No}) ->
    webatm:do(Name, [
        webatm:display("Session Timed Out."),
        webatm:eject()
    ]),
    backend:eject(No),
    {next_state, idle, #state{name=Name}}.

%%%%%
%% @spec handle_event(Call::term(), StateName::stateName(), state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_event(Event, _, State) ->
    {stop, {"Can not handle event in state", Event}, State}.

%%%%%
%% @spec idle(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(stop), Reason::string(), state()}
%
idle(Event, _, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

%%%%%
%% @spec get_pin(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(stop), Reason::string(), state()}
%
get_pin(Event, _, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

%%%%%
%% @spec selection(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(stop), Reason::string(), state()}
%
selection(Event, _, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

%%%%%
%% @spec withdraw(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(stop), Reason::string(), state()}
%
withdraw(Event, _, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

timeout(Event, _, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

%%%%%
%% @spec handle_sync_event(Call::term(), 
%%                         From::{pid(), reference()}, 
%%                         StateName::stateName(),
%%                         state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_sync_event(Event, _, _StateName, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

%%%%%
%% @spec handle_info(term(), StateName::atom(), state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_info(Info, _, State) ->
    {stop, {"Can not handle info", Info}, State}.

%%%%%
%% @spec code_change(OldVsn::term(), 
%%                   StateName::atom(), 
%%                   state(),
%%                   Extra::[term()]) ->
%%         {atom(ok), state()}
%
code_change(_, StateName, State, _) -> {ok, StateName, State}.

%%%%%
%% @terminate(Reason::term(), StateName::atom(), State::state()) -> none()
%
terminate(_, _, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec clear(state()) -> {atom(next_state), stateName(), state()}
%
clear(StateName, State) ->
    webatm:do(State#state.name, [webatm:display(" ")]),
    case StateName of
        idle -> {next_state, StateName, State#state{input = []}}; % no limit on idle
        StateName -> {next_state, StateName, State#state{input = []}, ?TIME_LIMIT}
    end.

%%%%%
%% @spec cancel(state()) -> {atom(next_state), atom(idle), state()}
%
cancel(#state{name=Name, accountNo=No}) ->
    webatm:do(Name, [
        webatm:display("cancel: Cancel button pressed"),
        webatm:eject()
    ]),
    backend:eject(No),
    {next_state, idle, #state{name=Name}}.

balance(#state{accountNo = No, pin = Pin}) ->
    [webatm:display("Balance:"), webatm:append_line("-------------------------"),
     webatm:append_line(io_lib:format("£ ~p", [backend:balance(No, Pin)]))].

mini_statement(#state{accountNo = No, pin = Pin}) ->
    Trs = backend:transactions(No, Pin),
    Balance = backend:balance(No, Pin),
    Trs1 = select10(Trs, [], 9),
    Trs2 =
        lists:map(fun({Type, {Year, Month, Day}, Sum}) ->
                    Con = case Type of
                        deposit -> "";
                        withdraw -> "-"
                    end,
                    webatm:append_line(
                        io_lib:format("~p/~p/~p ~s ~p~n", [Day, Month, Year, Con, Sum])
                    )
            end,
            Trs1),
    [webatm:display("Mini Statement:"), webatm:append_line("---------------------"),
        Trs2, webatm:append_line(io_lib:format("Balance: £ ~p", [Balance]))].

%%%%%
%% @spec select10([backend:account()], integer()) -> [backend:account()]
%
select10([], Acc, _) -> Acc;
select10(_, Acc, 0) -> Acc;
select10([H | T], Acc, N) -> select10(T, [H | Acc], N - 1).
