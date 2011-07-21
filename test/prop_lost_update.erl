-module(prop_lost_update).

-author('shino.shun@gmail.com').

-behaviour('proper_statem').

-export([initial_state/0,
         command/1,
         precondition/2,
         next_state/3,
         postcondition/3]).

-export([init/0,
         increment/1,
         measure/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {counter :: integer()}).

%% This property SHOULD FAIL.
prop_lost_update() ->
  numtests(1, 
           ?FORALL(Cmds, parallel_commands(?MODULE),
                   begin
                     {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
                     ?WHENFAIL(io:format("History: ~p\nState: ~p\nRes: ~p\n",
                                         [History, State, Result]),
                               Result =:= ok)
                   end)).

initial_state() ->
  ?MODULE:init(),
  #state{counter = 0}.

command(#state{counter = _Count}) ->
  oneof(
    [
%%      {call, ?MODULE, increment, [dirty]},
     %% If use atomic increment instead of dirty, the prop_lost_update should hold
     {call, ?MODULE, increment, [atomic]},
     {call, ?MODULE, measure, []}
    ]
   ).

%% Call -> {call, Module, Function, Args}
precondition(_State, _Call) ->
  true.

%% What post conditions mean?? Does at least one correct interleave suffice?
%% In other words, in what context the post conditions are evaluated?
postcondition(#state{counter = _Count}, {call, ?MODULE, increment, _}, _Result) ->
  ?debugVal([self(), postcondition, _Count]),
  true;
postcondition(#state{counter = Count}, {call, ?MODULE, measure, []}, Result) ->
  Res = Result =:= Count,
  ?debugVal([self(), postcondition, Count, Result, Res]),
  Res.

next_state(#state{counter = Count} = State, _Var, {call, ?MODULE, increment, _}) -> 
  ?debugVal([self(), next_state, Count]),
  State#state{counter = Count + 1};
next_state(State, _Var, {call, ?MODULE, measure, []}) -> 
  ?debugVal([self(), next_state, State#state.counter]),
  State.

-define(TAB, lost_update_counter).
-record(counter, {key :: atom(),
                  count :: integer()}).

init() ->
  ok = mnesia:start(),
  case mnesia:create_table(?TAB, [{record_name, counter},
                                  {attributes, record_info(fields, counter)}]) of
    {atomic, ok} ->
      ok = mnesia:dirty_write(?TAB, #counter{key = key, count = 0});
    {aborted, {already_exists, _}} -> 
      timer:sleep(1),
      ok
  end.

increment(dirty) ->
  [#counter{count =Count} = Record] = mnesia:dirty_read(?TAB, key),
  timer:sleep(2),
  ok = mnesia:dirty_write(?TAB, Record#counter{count = Count + 1}),
  ?debugVal({self(), increment, Count + 1}),
  ok;
increment(atomic) ->
  _NewVal = mnesia:dirty_update_counter(?TAB, key, 1),
  ?debugVal({self(), increment, _NewVal}),
  ok.

measure() ->
  [#counter{count =Count}] = mnesia:dirty_read(?TAB, key),
  Count.
