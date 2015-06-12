-module(db_list).
-compile(export_all).

-include("../include/db_list.hrl").

empty() -> [].

insert(Account, Db) ->
  Result = lookup(Account#account.no, Db),
  case Result of
    Account -> {error, exists};
    _ -> [Account|Db]
  end.

db_to_list(Db) -> Db.

db_size(Db) -> length(Db).

lookup(AccountNumber, Db) when is_integer(AccountNumber) ->
  Results = lookup_all(#account.no, AccountNumber, Db),
  case Results of
    [Account] -> Account;
    [] -> {error, no_account}
  end.

lookup_all(AccountField, Value, Db) -> 
  Results = lists:filter(fun (Account) ->
                             Value == element(AccountField, Account)
                         end, Db),
  Results.

update(Account, Db) -> 
  Results = lookup_all(#account.no, Account#account.no, Db),
  case Results of
    [OldAccount] ->
      NewDb = lists:delete(OldAccount, Db),
      [Account|NewDb];
    _ ->
      Db
  end.

close(_Db) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : db_list.erl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(db_list).
-export([empty/0, insert/2, db_to_list/1, db_size/1,
         lookup/2, lookup_all/3, update/2, close/1]).

-type no() :: pos_integer().
-type balance() :: number().
-type pin() :: string().
-type name() :: string() | binary().
-type transactions() :: list().

-record(account, {no :: no(),
                  balance=0 :: balance(),
                  pin :: pin(),
                  name :: name(),
                  transactions=[] :: transactions()}).

-type db() :: list(#account{}).


%% Returns an empty database
-spec empty() -> db().
empty() -> [].

%% Converts the database to a list
-spec db_to_list(db()) -> list(#account{}).
db_to_list(L) -> L.

%% Finds a given account in the database
-spec lookup(no(), db()) -> #account{} | {error, instance}.
lookup(_, []) -> {error, instance};
lookup(N, [A = #account{no=N} | _Rest]) -> A;
lookup(N, [_ | Rest]) -> lookup(N, Rest).

%% Finds a given account in the database, based on the #account{} record position
-spec lookup_all(no() | balance() | pin() | name() | transactions(), 2..6, db()) -> [#account{}].
lookup_all(_N, _Key, []) -> [];
lookup_all(N, Key, [Rec|Db]) ->
    if element(N,Rec) =:= Key -> [Rec | lookup_all(N, Key, Db)];
       element(N,Rec) =/= Key -> lookup_all(N, Key, Db)
    end.

%% Adds a new record to the DB
-spec insert(#account{}, db()) -> db().
insert(A = #account{}, []) ->
    [A];
insert(#account{no=N}, [#account{no=N} | _Rest]) ->
    {error, exists};
insert(A = #account{}, [Current | Rest]) ->
    [Current | insert(A, Rest)].

%% Updates a record in the DB
-spec update(#account{}, db()) -> db().
update(A = #account{}, []) -> [A];
update(A = #account{}, [Current|Rest]) ->
    if A#account.no =:= Current#account.no -> [A|Rest];
       A#account.no =/= Current#account.no -> [Current|update(A, Rest)]
    end.

%% Returns the size of the database
-spec db_size(db()) -> non_neg_integer().
db_size(L) -> length(L).

%% Destroys the DB.
-spec close(db()) -> ok.
close(_) -> ok.
