-module(prop).

-include_lib("../src/hash.hrl").
-include_lib("proper/include/proper.hrl").

-define(FORALL, macro_body).

fill_set(#hash_set{} = Set, [])-> Set;
fill_set(#hash_set{} = Set, [Hi | Ti]) ->
    fill_set(hash:insert(Hi, Set), Ti).

%% Проверка ассоциативности объединения
prop_associativity_test() ->
    ?FORALL({Keys1, Keys2, Keys3},
            {list(int()), list(int()), list(int())},
            begin
                Set1 = hashmap_set:new(),
                FilledSet1 = fill_set(Set1, Keys1),
                Set2 = hashmap_set:new(),
                FilledSet2 = fill_set(Set2, Keys2),
                Set3 = hashmap_set:new(),
                FilledSet3 = fill_set(Set3, Keys3),

                Union1 =
                    hashmap_set:union(
                        hashmap_set:union(FilledSet1, FilledSet2), FilledSet3),
                Union2 =
                    hashmap_set:union(
                        hashmap_set:union(FilledSet2, FilledSet3), FilledSet1),
                Union1 == Union2
            end).

%% Проверка нейтрального элемента
prop_neutral_element_test() ->
    ?FORALL({Key},
            {int()},
            begin
                % нейтральный элемент
                Empty = hashmap_set:new(),
                Set = hashmap_set:new(),
                FilledSet = hashmap_set:insert(Key, Set),

                Union1 = hashmap_set:union(Empty, FilledSet),
                Union2 = hashmap_set:union(FilledSet, Empty),
                (Union1 == Union2),
                (Union1 == FilledSet)
            end).

%% Проверка удаления элемента
prop_remove_test() ->
    ?FORALL({Key},
            {int()},
            begin
                Set = hashmap_set:new(),
                UpdatedSet = hashmap_set:insert(Key, Set),
                RemovedSet = hashmap_set:remove(Key, UpdatedSet),
                case hashmap_set:has_key(RemovedSet, Key) of
                    false ->
                        true;
                    true ->
                        false
                end
            end).
