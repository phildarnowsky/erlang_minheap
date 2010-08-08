-module(minheap).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

new() -> {minheap, array:new()}.
new(List) -> 
  SortedList = lists:keysort(1, List),
  {minheap, array:from_list(SortedList)}.

toList({minheap, HeapArray}) -> array:to_list(HeapArray).

empty({minheap, HeapArray}) -> array:sparse_size(HeapArray) =:= 0.

heap_size({minheap, HeapArray}) -> array:sparse_size(HeapArray).

parentIndex(N) -> (N - 1) div 2.
rightIndex(N) -> N * 2 + 1.
leftIndex(N) -> N * 2 + 2.

peek(Heap={minheap, HeapArray}) ->
  case empty(Heap) of
    true  -> nothing;
    false ->
      {Priority, Value} = array:get(0, HeapArray),
      {item, Priority, Value}
  end.

extract(Heap={minheap, HeapArray}) ->
  TopElement = peek(Heap),

  case TopElement of
    nothing -> 
      {nothing, Heap};
    _ ->
      LastIndex = heap_size(Heap) - 1,
      LastElement = array:get(LastIndex, HeapArray),

      HeapArray2 = array:set(0, LastElement, array:set(LastIndex, undefined, HeapArray)),
      HeapArray3 = reheap(0, HeapArray2),
      {TopElement, {minheap, HeapArray3}}
  end.

insert(Priority, Value, Heap={minheap, HeapArray}) ->
  Size = heap_size(Heap),
  HeapArray2 = array:set(Size, {Priority, Value}, HeapArray),
  HeapArray3 = bubble_up(Size, HeapArray2),
  {minheap, HeapArray3}.

swapArrayElements(I, J, Array) ->
  Tmp = array:get(I, Array),
  Array2 = array:set(I, array:get(J, Array), Array),
  array:set(J, Tmp, Array2).

bubble_up(0, HeapArray) -> HeapArray;
bubble_up(CurrentIndex, HeapArray) -> 
  ParentIndex = parentIndex(CurrentIndex),

  {CurrentPriority, _} = array:get(CurrentIndex, HeapArray),
  {ParentPriority, _} = array:get(ParentIndex, HeapArray),

  if
    ParentPriority =< CurrentPriority ->
      HeapArray;
    true ->
      HeapArray2 = swapArrayElements(CurrentIndex, ParentIndex, HeapArray),
      bubble_up(ParentIndex, HeapArray2)
  end.

reheap(ParentIndex, HeapArray) ->
  RightIndex = rightIndex(ParentIndex),
  LeftIndex = leftIndex(ParentIndex),

  SmallestPriorityIndex = smallestPriorityIndex([ParentIndex, RightIndex, LeftIndex], HeapArray),

  if
    SmallestPriorityIndex =:= ParentIndex ->
      HeapArray;
    true ->
      HeapArray2 = swapArrayElements(ParentIndex, SmallestPriorityIndex, HeapArray),
      reheap(SmallestPriorityIndex, HeapArray2)
  end.

smallestPriorityIndex([InitialIndex|Indices], Array) ->
  InitialPriority = priorityAtIndex(InitialIndex, Array),

  Champion = lists:foldl(
    fun(CurrentIndex, CurrentChampion={_SmallestIndex, SmallestPriority}) ->
      CurrentPriority = priorityAtIndex(CurrentIndex, Array),
      SmallerPriority = comparePriorities(SmallestPriority, CurrentPriority),

      if
        SmallerPriority =:= SmallestPriority ->
          CurrentChampion;
        true ->
          {CurrentIndex, CurrentPriority}
      end
    end,

    {InitialIndex, InitialPriority},
    Indices
  ),
  
  {SmallestIndex, _SmallestPriority} = Champion,
  SmallestIndex.

priorityAtIndex(Index, Array) ->
  case array:get(Index, Array) of
    {Priority, _Value} -> Priority;
    undefined -> undefined
  end.

comparePriorities(A, undefined) -> A;
comparePriorities(A, B) ->
  if 
    A > B -> B;
    true -> A
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

  
extract_option(OptionName, Default, PropList) ->
  case lists:keysearch(OptionName, 1, PropList) of
    {value, {OptionName, Value}} -> Value;
    false -> Default
  end.

new_0_returns_empty_minheap_test() ->
  ?assert(empty(new())).

new_1_returns_minheap_with_equivalent_length_test() ->
  Heap = new([{3, foo}, {2, bar}, {1, baz}]),
  ?assert(heap_size(Heap) =:= 3).

test_on_random_heaps(TestFun) -> test_on_random_heaps(TestFun, []).
test_on_random_heaps(TestFun, Options) ->
  HeapCount = extract_option(heap_count, 100, Options),
  HeapSize = extract_option(heap_size, 100, Options),
  HeapSizeFun = extract_option(heap_size_fun, null, Options),
  HeapElementFun = extract_option(heap_element_fun, fun random_heap_tuple/1, Options),

  lists:foreach(
    fun(_I) ->
      CurrentHeapSize = if 
        HeapSizeFun /= null ->
          HeapSizeFun();
        true ->
          HeapSize
      end,

      List = lists:map(
        HeapElementFun,
        lists:seq(1, CurrentHeapSize)
      ),

      Heap = new(List),
      TestFun(Heap)
    end,

    lists:seq(1, HeapCount)
  ).

random_heap_tuple(I) -> {random:uniform(500), I}.

new_minheap_has_minheap_property_test() ->
  test_on_random_heaps(fun assert_has_minheap_property/1). 

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

extract_from_empty_heap_returns_nothing_test() ->
  Heap = new(),
  ?assert(empty(Heap)),
  ?assert(extract(Heap) =:= {nothing, Heap}).

extract_from_nonempty_heap_returns_top_element_and_new_heap_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      OriginalSize = heap_size(Heap),
      {item, TopPriority, TopValue} = peek(Heap),

      {{item, TopPriority, TopValue}, Heap2} = extract(Heap),

      ?assert(heap_size(Heap2) =:= OriginalSize - 1),
      assert_has_minheap_property(Heap2)
    end
  ).

extract_from_nonempty_heap_has_all_elements_but_former_top_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      OriginalList = toList(Heap),
      {{item, TopPriority, TopValue}, Heap2} = extract(Heap),
      ExtractedList = toList(Heap2),

      ListDifference = OriginalList -- ExtractedList,
      ListDifference = [{TopPriority, TopValue}]
    end
  ).

to_list_is_inverse_of_new_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      Clone = new(toList(Heap)),
      ?assert(Heap =:= Clone)
    end
  ).
