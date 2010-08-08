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

peek(Heap={minheap, HeapArray}) ->
  case empty(Heap) of
    true  -> nothing;
    false ->
      {Priority, Value} = array:get(0, HeapArray),
      {item, Priority, Value}
  end.

insert(Priority, Value, Heap={minheap, HeapArray}) ->
  Size = heap_size(Heap),
  HeapArray2 = array:set(Size, {Priority, Value}, HeapArray),
  HeapArray3 = bubble_up(Size, HeapArray2),
  {minheap, HeapArray3}.

bubble_up(0, HeapArray) -> HeapArray;
bubble_up(I, HeapArray) -> 
  ParentIndex = parentIndex(I),

  CurrentTuple = {CurrentPriority, _} = array:get(I, HeapArray),
  ParentTuple = {ParentPriority, _} = array:get(ParentIndex, HeapArray),

  if
    ParentPriority =< CurrentPriority ->
      HeapArray;
    true ->
      HeapArray2 = array:set(ParentIndex, CurrentTuple, (array:set(I, ParentTuple, HeapArray))),
      bubble_up(ParentIndex, HeapArray2)
  end.


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

peek_on_empty_minheap_returns_nothing_test() ->
  ?assert(peek(new()) =:= nothing).

peek_on_nonempty_miheap_returns_proper_tuple_test() ->
  Heap = new([{4, quux}, {3, baz}, {2, bar}, {1, foo}]),
  ?assert(peek(Heap) =:= {item, 1, foo}).

insert_into_empty_heap_has_one_element_at_top_test() ->
  Heap = new(),
  ?assert(empty(Heap)),
  Heap2 = insert(5, foo, Heap),
  ?assert(heap_size(Heap2) =:= 1),
  ?assert(peek(Heap2) =:= {item, 5, foo}).

insert_of_new_minimum_value_bubbles_to_top_test() ->
  Heap = new([{4, quux}, {3, baz}, {2, bar}, {1, foo}]),
  NewHeap = insert(0, fnord, Heap),
  assert_has_minheap_property(NewHeap),
  ?assert(heap_size(NewHeap) =:= 5).
