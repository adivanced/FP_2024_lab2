-module(unit).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/hash.hrl").

-define(assertEqual, macro_body).
-define(assert, macro_body).

new_set_test() ->
    Set = hash:new(10),
    ?assertEqual(10, Set#hash_set.capacity),
    Set1 = hash:new(20),
    ?assertEqual(20, Set1#hash_set.capacity).

insert_elem_test() ->
    Set1 = hash:new(16),
    Set2 = hash:insert(5, Set1),
    Set3 = hash:insert(221312, Set2),
    Set4 = hash:insert(999, Set3),
    ?assertEqual(3, Set4#hash_set.size),
    ?assertEqual(true, hash:has_key(Set4, 5)),
    ?assertEqual(true, hash:has_key(Set4, 221312)),
    ?assertEqual(true, hash:has_key(Set4, 999)).

remove_element_test() ->
    Set1 = hash:new(16),
    Set2 = hash:insert(9, Set1),
    Set3 = hash:insert(8, Set2),
    Set4 = hash:insert(7, Set3),
    Set5 = hash:insert(10, Set4),
    Set6 = hash:remove(9, Set5),
    Set7 = hash:remove(8, Set6),
    ?assertEqual(true, hash:has_key(Set7, 7)),
    ?assertEqual(false, hash:has_key(Set7, 9)).

filter_test() ->
    Set0 = hash:new(),
    Set1 = hash:insert(9, Set0),
    Set2 = hash:insert(8, Set1),
    Set3 = hash:insert(7, Set2),
    Set4 = hash:insert(10, Set3),
    Set5 = hash:filter(fun(X) -> X > 7 end, Set4),
    ?assertEqual(3, Set5#hash_set.size),
    ?assertEqual(false, hash:has_key(Set5, 7)).

from_to_list_test() ->
    Lst = [1,2,3,4,4,5,5,5,6],
    Set = hash:from_list(Lst),
    LstNew = hash:to_list(Set),
    Answ = LstNew =:= Lst,
    ?assertEqual(true, Answ).

foldl_set_test() ->
    Lst = [1, 2, 3, 4, 5, 6, 4, 5, 5],
    Set = hash:from_list(Lst),
    Val = hash:foldl(Set, fun(X, Sum) -> X + Sum end, 0),
    ?assertEqual(35, Val).

foldr_set_test() ->
    Lst = [1, 2, 3, 4, 5, 6, 4, 5, 5],
    Set = hash:from_list(Lst),
    Val = hash:foldr(Set, fun(X, Sum) -> X + Sum end, 0),
    ?assertEqual(35, Val).

map_set_test() ->
    Set0 = hash:new(),
    Set1 = hash:insert(3, Set0),
    Set2 = hash:insert(7, Set1),
    Set3 = hash:insert(22, Set2),
    Set4 = hash:insert(15, Set3),
    Set5 = hash:map(fun(X) -> X * X end, Set4),

    ?assert(hash:has_key(Set5, 9)),

    ?assert(hash:has_key(Set5, 49)),

    ?assert(hash:has_key(Set5, 484)),

    ?assert(hash:has_key(Set5, 225)).

has_a_neutral_element_test() ->
    NeutralElement = hash:new(),
    SetAdd = hash:insert(5, NeutralElement),
    ?assertEqual(SetAdd, hash:insert(5, NeutralElement)).

