%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : ebank_sup.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2012 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ebank_sup).
-vsn('2.0').
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0, start_atm/2, stop_atm/1 ]).

%%%%%
% Gen supervisor part
%
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

start_atm(Name, Port) -> supervisor:start_child(?MODULE, child(atm_sup, Name, [Port])).

stop_atm(Name) ->
    supervisor:terminate_child(?MODULE, {atm_sup, Name}),
    supervisor:delete_child(?MODULE, {atm_sup, Name}).

init(no_args) ->
    {ok, {{rest_for_one, 5, 2000}, [child(backend, none)]}}.

child(Module, none) ->
    {Module, {Module, start_link, []}, permanent, brutal_kill, worker, [Module]};
child(Module, Name) ->
    {{Module, Name}, {Module, start_link, [Name]},
        permanent, brutal_kill, worker, [Module]}.
child(Module, Name, Args) ->
    {{Module, Name}, {Module, start_link, [Name] ++ Args},
        permanent, brutal_kill, worker, [Module]}.
