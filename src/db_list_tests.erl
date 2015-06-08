-module(db_list_tests).
-compile(export_all).

-include("../include/db_list.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(test(C, F), {C, ?setup(F)}).

%%
%% DESCRIPTIONS
%%

db_api_test_() ->
  [
   ?test("empty creates an empty database", fun empty_test/1),
   ?test("db_to_list returns the db as a list", fun db_to_list_test/1),
   ?test("db_size returns the size of the list", fun db_size_test/1),
   ?test("close always returns ok", fun db_size_test/1)
  ].

db_records_test_() ->
  [
   ?test("can insert new account", fun insert_new_test/1),
   ?test("can't insert duplicated account", fun insert_duplicated_test/1),
   ?test("can lookup by account number", fun lookup_account_number_test/1),
   ?test("can lookup by any field", fun lookup_by_field_test/1),
   ?test("can update existing account", fun update_account_test/1),
   ?test("updating unknown account returns the db", fun update_unknown_account_test/1)
  ].

%%
%% SETUP
%%

start() -> 
  Db = db_list:empty(),
  Db.

stop(_) -> ok.

%% 
%% ASSERTIONS
%%

empty_test(Db) ->
  [ ?_assertEqual( Db, [] ) ].

db_to_list_test(Db) ->
  [ ?_assertEqual( Db, [] )].

db_size_test(Db) ->
  Size = db_list:db_size(Db),
  [ ?_assertEqual( Size, 0 )].

close_test(Db) -> 
  [ ?_assert( db_list:close(Db) )].

insert_new_test(Db) ->
  Record = new_record(),
  NewDb = db_list:insert(Record, Db),
  [ ?_assertEqual( NewDb, [Record] )].

insert_duplicated_test(Db) ->
  Record = new_record(),
  NewDb = db_list:insert(Record, Db),
  {error, Reason} = db_list:insert(Record, NewDb),
  [ ?_assertEqual( Reason, exists )].

lookup_account_number_test(Db) -> 
  Record = new_record(),
  NewDb = db_list:insert(Record, Db),
  Found = db_list:lookup(42, NewDb),
  [ ?_assertEqual( Found, Record )].

lookup_by_field_test(Db) ->
  Record = new_record(),
  NewDb = db_list:insert(Record, Db),
  [Found] = db_list:lookup_all(#account.no, 42, NewDb),
  [ ?_assertEqual( Found, Record )].

update_account_test(Db) ->
  Record = new_record(),
  NewDb = db_list:insert(Record, Db),
  NewRecord = Record#account{name="Tony Stark"},
  UpdatedDb = db_list:update(NewRecord, NewDb),
  [Found] = db_list:lookup_all(#account.no, 42, UpdatedDb),
  [ ?_assertEqual( Found#account.name, NewRecord#account.name )].

update_unknown_account_test(Db) ->
  Record = new_record(),
  NewDb = db_list:update(Record, Db),
  [ ?_assertEqual( NewDb, [] )].

%%
%% HELPER METHODS
%%

new_record() ->
  #account{no=42, name="Yay!"}.
