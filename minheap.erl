-module(minheap).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

new() -> {minheap, array:new()}.
new(List) -> 
  SortedList = lists:keysort(1, List),
  {minheap, array:from_list(SortedList)}.

empty({minheap, HeapArray}) -> array:sparse_size(HeapArray) =:= 0.
heap_size({minheap, HeapArray}) -> array:sparse_size(HeapArray).

parentIndex(N) -> (N - 1) div 2.

%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%

assert_has_minheap_property(Heap={minheap, HeapArray}) ->
  lists:foreach(
    fun(I) -> 
        {CurrentPriority, _} = array:get(I, HeapArray),
        {ParentPriority, _} = array:get(parentIndex(I), HeapArray),
        ?assert(ParentPriority =< CurrentPriority)
    end,
    lists:seq(2, heap_size(Heap) - 1)
  ).

new_0_returns_empty_minheap_test() ->
  ?assert(empty(new())).

new_1_returns_minheap_with_equivalent_length_test() ->
  Heap = new([{3, foo}, {2, bar}, {1, baz}]),
  ?assert(heap_size(Heap) =:= 3).

new_minheap_has_minheap_property_test() ->
  Iterations = lists:seq(1, 100),
  lists:foreach(
    fun(_I) ->
        ListLength = random:uniform(100) + 10,
        List = lists:map(
          fun(J) -> {random:uniform(500), J} end,
          lists:seq(1, ListLength)
        ),
        Heap = new(List),
        assert_has_minheap_property(Heap)
    end,

    Iterations
  ).
