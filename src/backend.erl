%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : backend.erl
<<<<<<< HEAD
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
=======

>>>>>>> 10dedf5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(backend).
<<<<<<< HEAD

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
=======
-vsn('1.1').
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("backend.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
% Interface
%
-export([start_link/0, stop/0,
	 account/1, pin_valid/2, change_pin/3,
	 balance/2, transactions/2,
	 withdraw/3, deposit/2,
	 transfer/4,
         block/1,
         eject/1
	]).


%%%%%
% Gen Server part
%
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

>>>>>>> 10dedf5

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DB, db_list).
-define(ACCOUNTS,
	[{1, 100, "1234", "Henry Nystrom"},
	 {2, 200, "4321", "Fransceco Cesarini"},
	 {3, 1000, "1111", "Donald Duck"},
	 {4, 5000, "1234", "Henry Nystrom"}
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<<<<<<< HEAD
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
=======
%%%%%
%% @type state() = {db_list:db(), [accountNo()]}
%%
%% @type account() = {accountNo(),
%%                    balance(),
%%                    pin(),
%%                    name(),
%%                    [transaction()],
%%                    integer()
%%                    }
%%
%% @type accountNo() = integer()
%% @type balance() = integer()
%% @type pin() = string()
%% @type name() = string()
%% @type transaction() = {atom(), date(), Amount::integer()}
%% @type date() = {Year::integer(), Month::integer(), Day::integer()}
%%                    
%
-record(state, {accounts,
                blocked = [],
                alarms = []}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec start_link() -> {atom(ok), pid()} | {error, Reason::term()}
%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).


%%%%%
%% @spec stop() -> atom(ok)
%
stop() -> gen_server:call(?MODULE, stop).

%%%%%
%% @spec account(atom() | accountNo()) -> [account()]
%
account(Account) -> gen_server:call(?MODULE, {account, Account}).
>>>>>>> 10dedf5

account(Account) -> 
  gen_server:call(?MODULE, {account, Account}).

<<<<<<< HEAD
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
  {ok, BlockedAccount, NewState} = do_block(Account, State),
  Reply = {ok, BlockedAccount},
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
=======
%%%%%
%% @spec pin_valid(accountNo(), string()) -> bool()
%
pin_valid(AccountNo, Input) ->
  gen_server:call(?MODULE, {is_pin_valid, AccountNo, Input}).

%%%%%
%% @spec change_pin(string(), string(), string()) -> bool()
%
change_pin(User, OldPin, NewPin) ->
  gen_server:call(?MODULE, {change_pin, User, OldPin, NewPin}).


%%%%%
%% @spec withdraw(accountNo(), string(), integer()) ->
%%         atom(ok) | {atom(error), string()}
%
withdraw(AccountNo, Pin, Amount) ->
  gen_server:call(?MODULE, {withdraw, AccountNo, Pin, Amount}).

%%%%%
%% @spec deposit(accountNo(), integer()) ->
%%         atom(ok) | {atom(error), string()}
%
deposit(AccountNo, Amount) ->
  gen_server:call(?MODULE, {deposit, AccountNo, Amount}).

%%%%%
%% @spec transfer(accountNo(), string(), integer()) ->
%%         atom(ok) | {atom(error), string()}
%
transfer(Amount, From, To, Pin) ->
  gen_server:call(?MODULE, {transfer, From, To, Pin, Amount}).

%%%%%
%% @spec block(accountNo()) ->  atom(ok) | {atom(error), string()}
%
block(AccountNo) -> gen_server:call(?MODULE, {block, AccountNo}).

%%%%%
%% @spec eject(accountNo()) ->  atom(ok) | {atom(error), string()}
%
eject(AccountNo) -> gen_server:call(?MODULE, {eject, AccountNo}).


%%%%%
%% @spec balance(accountNo(), integer()) ->
%%         atom(ok) | {atom(error), string()}
%
balance(AccountNo, Pin) ->
  gen_server:call(?MODULE, {balance, AccountNo, Pin}).

%%%%%
%% @spec balance(accountNo(), integer()) ->
%%         atom(ok) | {atom(error), string()}
%
transactions(AccountNo, Pin) ->
  gen_server:call(?MODULE, {transactions, AccountNo, Pin}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS/GEN_SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec init(atom(no_args)) -> {ok, state()}
%
init(no_args) ->
  process_flag(trap_exit, true),
  Accounts =
    lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
		    ?DB:insert(new_account(No, Balance, Pin, Name), DB)
		end,
		?DB:empty(),
		?ACCOUNTS),
  {ok, #state{accounts = Accounts}}.

>>>>>>> 10dedf5

%%%%%
%% @spec handle_call(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(reply), Reply::term(), state()}
%
handle_call({account, Accounts}, _, State) ->
  Reply = case Accounts of
	    all ->
	      lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
			?DB:db_to_list(State#state.accounts));
	    Name when list(Name) -> findAccount(Name, State);
	    No when integer(No) -> [findAccount(No, State)]
	  end,
  {reply, Reply, State};

handle_call({is_pin_valid, AccountNumber, Pin}, {Pid, _}, State) -> %%CHANGED
  Blocked = lists:member(AccountNumber, State#state.blocked),
  Alarmed = lists:member(AccountNumber, State#state.alarms),
  case {Blocked, Alarmed} of
    {true, true} -> {reply, true, State};
    {true, false} ->
      Whom = process_info(Pid, registered_name),
      alarm_handler:set_alarm({AccountNumber, Whom}),
      State1 = State#state{alarms = [AccountNumber | State#state.alarms]},
      {reply, true, State1};
    {false, _} ->
      Account = findAccount(AccountNumber, State),
      Bool = is_pin_valid(Account, Pin),
      stats:log({login, AccountNumber, Bool}),
      {reply, Bool, State}
  end;
handle_call({new_account, [Balance, Pin, Name]}, _, State) ->
  Accounts = State#state.accounts,
  No = ?DB:db_size(Accounts),
  NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
  {reply, ok, State#state{accounts = NewAccounts}};
handle_call({balance, AccountN, Pin}, _, State) ->
  case lists:member(AccountN, State#state.blocked) of
    true -> {reply, 0, State};
    false -> {reply, balance(AccountN, Pin, State), State}
  end;
handle_call({transactions, AccountN, Pin}, _, State) ->
    case lists:member(AccountN, State#state.blocked) of
        true -> {reply, [], State};
        false -> {reply, transactions(AccountN, Pin, State), State}
    end;
handle_call({withdraw, FromAccountN, Pin, Amount}, _, State) ->
  case lists:member(FromAccountN, State#state.blocked) of 
    true -> {reply, {error, "Not enough money on account!"}, State};
    false ->
      case withdraw(FromAccountN, Pin, Amount, State) of
	{ok, NewState} ->
              stats:log({withdraw, FromAccountN, Amount}),
              {reply, ok, NewState};
	{error, Reason} -> {reply, {error, Reason}, State}
      end
  end;
handle_call({deposit, ToAccountN, Amount}, _, State) ->
  case deposit(ToAccountN, Amount, State) of
    {ok, NewState} ->
          stats:log({deposit, ToAccountN, Amount}),
          {reply, ok, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;
handle_call({transfer, FromAccountN, ToAccountN, Pin, Amount}, _, State) ->
  case transfer(FromAccountN, ToAccountN, Pin, Amount, State) of
    {ok, NewState} ->
          stats:log({transfer, FromAccountN, ToAccountN, Amount}),
          {reply, ok, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;
handle_call({change_pin, User, OldPin, NewPin}, _, State) ->
  case change_pin_i(User, OldPin, NewPin, State) of
    {ok, NewState} -> {reply, ok, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;
handle_call({block, AccountNo}, _, State) ->
  {reply, ok, block(AccountNo, State)};
handle_call({eject, AccountNo}, _, State) ->
  stats:log({eject, AccountNo}),
  {reply, ok, eject(AccountNo, State)};
handle_call(stop, _, State) ->
  {stop, normal, State}.

%%%%%
%% @spec handle_cast(Cast::term(), state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_cast(Cast, State) -> {stop, {"Can not handle cast", Cast}, State}.


%%%%%
%% @spec handle_info(term(), state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_info(Info, State) -> {stop, {"Can not handle info", Info}, State}.
  
 

%%%%%
%% @spec code_change(OldVsn::term(), state(), Extra::[term()]) ->
%%         {atom(ok), state()}
%
code_change(_, State, _) ->
  {ok, State}.

%%%%%
%% @terminate(Reason::term(), State::state()) -> none()
%
terminate(shutdown, State) -> ?DB:close(State#state.accounts);
terminate(_, _) -> ok.

   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec new_account(integer(), balance(), pin(), name()) -> account()
%
new_account(No, Balance, Pin, Name) ->
  #account{no = No, balance = Balance, pin = Pin, name = Name}.

%%%%%
%% @spec (accountNo(), state()) -> account()
%
findAccount(AccountN, State) when integer(AccountN) ->
  ?DB:lookup(AccountN, State#state.accounts);
findAccount(User, State) when list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

<<<<<<< HEAD
do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{transactions = OldTransactions} =
  find_account(AccountN, State),
  OldBalance = get_account_balance(Account),
  case do_pin_valid(Account, Pin) of
=======
%%%%%
%% @spec withdraw(accountNo(), pin(), integer(), state()) ->
%%         {atom(ok), state()} | {atom(error), Reason::string()}
%
withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    findAccount(AccountN, State),
  case is_pin_valid(Account, Pin) of
>>>>>>> 10dedf5
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


%%%%%
%% @spec deposit(accountNo(), integer(), state()) -> {atom(ok), state()}
%
deposit(AccountN, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
<<<<<<< HEAD
  find_account(AccountN, State),
=======
    findAccount(AccountN, State),
>>>>>>> 10dedf5
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
  Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts}}.

<<<<<<< HEAD
do_balance(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> get_account_balance(Account);
    false -> {error, "PIN code not valid!"}
  end.

do_transactions(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
=======
%%%%%
%% @spec balance(accountNo(), pin(), state()) -> Balance::integer()
%
balance(AccountN, Pin, State) ->
  Account = findAccount(AccountN, State),
  case is_pin_valid(Account, Pin) of
    true -> Account#account.balance;
    false -> {error, "PIN code not valid!"}
  end.
	
%%%%%
%% @spec transactions(accountNo(), pin(), state()) ->
%%         Transactions::[transaction()]
%
transactions(AccountN, Pin, State) ->
  Account = findAccount(AccountN, State),
  case is_pin_valid(Account, Pin) of
>>>>>>> 10dedf5
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

%%%%%
%% @spec transfer(accountNo(), accountNo(), pin(), integer(), state()) ->
%%         {atom(ok), state()} | {atom(error), Reason::string()}
%
transfer(FromAccountN, ToAccountN, Pin, Amount, State) ->
  case withdraw(FromAccountN, Pin, Amount, State) of
    {ok, NewState} -> deposit(ToAccountN, Amount, NewState);
    {error, Reason} -> {error, Reason}
  end.

%%%%%
%% @spec is_pin_valid(account() | [account], pin()) -> bool()
%
is_pin_valid([], _) -> false;
is_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
is_pin_valid(Account, Pin) -> Account#account.pin == Pin.

%%%%%
%% @spec change_pin_i(string(), string(), string()) -> bool()
%
change_pin_i(User, OldPin, NewPin, State) ->
  Accounts = findAccount(User, State),
  case is_pin_valid(Accounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      Accounts1 =
<<<<<<< HEAD
      lists:foldl(fun(Account, Acc) ->
                      ?DB:update(Account#account{pin = NewPin}, Acc)
                  end,
                  State#state.accounts,
                  Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.

do_block(Account=#account{blocked=false}, State) ->
  Accounts = State#state.accounts,
  BlockedAccount = Account#account{blocked=true},
  NewAccounts = ?DB:update(BlockedAccount, Accounts),
  NewState = State#state{accounts=NewAccounts},
  {ok, BlockedAccount, NewState}.

get_account_balance(#account{blocked=true}) ->
  default_blocked_balance();
get_account_balance(#account{balance=Balance}) ->
  Balance. 

default_blocked_balance() -> 10.

code_change(_, _, _) -> exit(not_implemented).

handle_cast({stop, _Reason}, _From) -> {stop, normal};
handle_cast(_, _) -> exit(not_implemented).

handle_info(_, _) -> exit(not_implemented).
=======
	lists:foldl(fun(Account, Acc) ->
			?DB:update(Account#account{pin = NewPin}, Acc)
		    end,
		    State#state.accounts,
		    Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.

%%%%%
%% @spec block(accountNo, state()) ->  state
%
block(AccountNo, State) ->
  State#state{blocked = [AccountNo | State#state.blocked]}.

%%%%%
%% @spec eject(accountNo(), state()) -> state()
%
eject(AccountNo, State) ->
  Alarms = State#state.alarms,
  case lists:member(AccountNo, Alarms) of
    true ->
      alarm_handler:clear_alarm(AccountNo),
      State#state{alarms = lists:delete(AccountNo, State#state.alarms)};
    false -> State
  end.
>>>>>>> 10dedf5
