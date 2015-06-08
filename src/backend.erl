%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : backend.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(backend).

-include("../include/backend.hrl").

-export([account/1, 
         balance/2, 
         block/1,
         change_pin/3,
         deposit/2, 
         pin_valid/2, 
         transactions/2,
         transfer/4,
         withdraw/3]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start/0,
         start_link/0, 
         stop/0, 
         terminate/2]).

-define(DB, db_list).
-define(ACCOUNTS,
        [{1, 100, "1234", "Henry Nystrom"},
         {2, 200, "4321", "Francesco Cesarini"},
         {3, 1000, "1111", "Donald Duck"},
         {4, 5000, "1234", "Henry Nystrom"}
        ]).

-record(state, {accounts}).

-behavior(gen_server).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

terminate(_Reason, _State) -> ok.

block(Account) ->
  gen_server:call(?MODULE, {block, Account}).

account(Account) -> 
  gen_server:call(?MODULE, {account, Account}).

pin_valid(AccountNo, Input) -> 
  gen_server:call(?MODULE, {pin_valid, AccountNo, Input}).

change_pin(User, OldPin, NewPin) -> 
  gen_server:call(?MODULE, {change_pin, User, OldPin, NewPin}).

withdraw(AccountNo, Pin, Amount) -> 
  gen_server:call(?MODULE, {withdraw, AccountNo, Pin, Amount}).

deposit(AccountNo, Amount) -> 
  gen_server:call(?MODULE, {deposit, AccountNo, Amount}).

transfer(Amount, From, To, Pin) -> 
  gen_server:call(?MODULE, {transfer, From, To, Pin, Amount}).

balance(AccountNo, Pin) -> 
  gen_server:call(?MODULE, {balance, AccountNo, Pin}).

transactions(AccountNo, Pin) -> 
  gen_server:call(?MODULE, {transactions, AccountNo, Pin}).

init(_Args) ->
  process_flag(trap_exit, true),
  Accounts = lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
                             ?DB:insert(new_account(No, Balance, Pin, Name), DB)
                         end,
                         ?DB:empty(),
                         ?ACCOUNTS),
  {ok, #state{accounts = Accounts}}.

handle_call({account, Accounts}, _From, State) ->
  Reply = case Accounts of
            all ->
              lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
                        ?DB:db_to_list(State#state.accounts));
            Name when is_list(Name) -> find_account(Name, State);
            No when is_integer(No) -> [find_account(No, State)]
          end,
  {reply, Reply, State};

handle_call({block, AccountNumber}, _From, State) ->
  Account = find_account(AccountNumber, State),
  Reply = {ok, BlockedAccount} = do_block(Account),
  NewState = ?DB:update(BlockedAccount, State#state.accounts),
  {reply, Reply, NewState};

handle_call({pin_valid, AccountNumber, Pin}, _From, State) ->
  Account = find_account(AccountNumber, State),
  Reply = do_pin_valid(Account, Pin),
  {reply, Reply, State};

handle_call({new_account, [Balance, Pin, Name]}, _From, State) ->
  Accounts = State#state.accounts,
  No = ?DB:db_size(Accounts) + 1,
  NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
  Reply = ok,
  NewState = State#state{accounts = NewAccounts},
  {reply, Reply, NewState};

handle_call({balance, AccountN, Pin}, _From, State) ->
  Reply = do_balance(AccountN, Pin, State),
  {reply, Reply, State};

handle_call({transactions, AccountN, Pin}, _From, State) ->
  Reply = do_transactions(AccountN, Pin, State),
  {reply, Reply, State};

handle_call({withdraw, FromAccountN, Pin, Amount}, _From, State) ->
  handle_action( do_withdraw(FromAccountN, Pin, Amount, State), State);

handle_call({deposit, ToAccountN, Amount}, _From, State) ->
  handle_action( do_deposit(ToAccountN, Amount, State), State);

handle_call({transfer, FromAccountN, ToAccountN, Pin, Amount}, _From, State) ->
  handle_action( do_transfer(FromAccountN, ToAccountN, Pin, Amount, State), State);

handle_call({change_pin, User, OldPin, NewPin}, _From, State) ->
  handle_action( do_change_pin(User, OldPin, NewPin, State), State).

handle_action({ok, NewState}, _State) -> {reply, ok, NewState};
handle_action({error, Reason}, State) -> {reply, {error, Reason}, State}.

new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, State) when is_integer(AccountN) ->
  ?DB:lookup(AccountN, State#state.accounts);
find_account(User, State) when is_list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
  find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    false -> {error, "PIN code not valid!"};
    true when OldBalance < Amount -> {error, "Not enough money on account!"};
    true ->
      NewBalance = OldBalance - Amount,
      NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
      AccountUpdated =
      Account#account{balance = NewBalance, transactions = NewTransactions},
      NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
      {ok, State#state{accounts = NewAccounts}}
  end.

do_deposit(AccountN, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
  find_account(AccountN, State),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
  Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts}}.

do_balance(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> get_account_balance(Account);
    false -> {error, "PIN code not valid!"}
  end.

do_transactions(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

do_transfer(FromAccountN, ToAccountN, Pin, Amount, State) ->
  case do_withdraw(FromAccountN, Pin, Amount, State) of
    {ok, NewState} -> do_deposit(ToAccountN, Amount, NewState);
    {error, Reason} -> {error, Reason}
  end.

do_pin_valid([], _) -> false;
do_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
do_pin_valid(Account, Pin) -> Account#account.pin == Pin.

do_change_pin(User, OldPin, NewPin, State) ->
  Accounts = find_account(User, State),
  case do_pin_valid(Accounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      Accounts1 =
      lists:foldl(fun(Account, Acc) ->
                      ?DB:update(Account#account{pin = NewPin}, Acc)
                  end,
                  State#state.accounts,
                  Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.

do_block(Account=#account{blocked=false}) ->
  BlockedAccount = Account#account{blocked=true},
  {ok, BlockedAccount}.

get_account_balance(#account{blocked=true}) ->
  default_blocked_balance();
get_account_balance(#account{balance=Balance}) ->
  Balance. 

default_blocked_balance() -> 10.

code_change(_, _, _) -> exit(not_implemented).

handle_cast({stop, _Reason}, _From) -> {stop, normal};
handle_cast(_, _) -> exit(not_implemented).

handle_info(_, _) -> exit(not_implemented).
