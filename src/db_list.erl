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
