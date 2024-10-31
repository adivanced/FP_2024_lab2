-module(hash).

-export([remove/2, insert/2, insertn/3, removen/3, map/2, union/2, has_key/2, to_list/1,
         from_list/1, amount_key/2, new/1, new/0, filter/2, foldl/3, foldr/3]).

-include("hash.hrl").

new(Capacity) ->
    #hash_set{size = 0,
              capacity = Capacity,
              %table = lists:duplicate(Capacity, [undefined, 0])
              table = fill_arr(Capacity * 2, array:new(Capacity * 2, {default, 0}))}.

fill_arr(0, Arr) ->
    Arr;
fill_arr(Cap, Arr) ->
    fill_arr(Cap - 2, array:set(Cap - 2, undefined, Arr)).

new() ->
    new(16).

hash(Key, Capacity) ->
    erlang:phash2(Key, Capacity).

get_element(Index, Table) ->
    array:get(Index * 2, Table).%lists:nth(Index+1, Table).

get_elementq(Index, Table) ->
    array:get(Index * 2 + 1, Table).

insert(Key,
       #hash_set{size = Size,
                 capacity = Capacity,
                 table = Table} =
           Set) ->
    case Size >= Capacity of
        true ->
            resize_and_insert(Key, Set);
        false ->
            Index = hash(Key, Capacity),
            A = get_element(Index, Table),
            B = get_elementq(Index, Table),
            %[A, B] = get_element(Index, Table),
            % if A == undefined ->
            % io:fwrite("UNDEF\n");
            % true -> io:fwrite(integer_to_list(A)), io:fwrite(" \n")
            % end,
            % io:fwrite(integer_to_list(B)),
            % io:fwrite(" \n"),
            %io:fwrite(integer_to_list(Index)),
            %io:fwrite(" \n"),
            %NewTable = lists:sublist(Table,Index) ++ [[Key, B+1]] ++ lists:nthtail(Index,Table),
            NewTable1 = array:set(Index * 2, Key, Table),
            NewTable2 = array:set(Index * 2 + 1, B + 1, NewTable1),

            % [A1, B1] = get_element(Index, NewTable),
            % io:fwrite(integer_to_list(A1)),
            % io:fwrite(" \n"),
            % io:fwrite(integer_to_list(B1)),
            % io:fwrite(" \n"),
            %Tmp = print_arr(NewTable2, 0, Capacity*2),
            Set#hash_set{size = newsize(Size, A),
                         capacity = Capacity,
                         table =
                             NewTable2}                        %make_set(Size+1, Capacity, NewTable)
    end.

insertn(Key,
        Amount,
        #hash_set{size = Size,
                  capacity = Capacity,
                  table = Table} =
            Set) ->
    case Size >= Capacity of
        true ->
            resize_and_insertn(Key, Amount, Set);
        false ->
            Index = hash(Key, Capacity),
            A = get_element(Index, Table),
            B = get_elementq(Index, Table),

            NewTable1 = array:set(Index * 2, Key, Table),
            NewTable2 = array:set(Index * 2 + 1, B + Amount, NewTable1),

            %Tmp = print_arr(NewTable2, 0, Capacity*2),
            Set#hash_set{size = newsize(Size, A),
                         capacity = Capacity,
                         table = NewTable2}
    end.

newsize(Size, A) ->
    if A == undefined ->
           Size + 1;
       true ->
           Size
    end.

resize_and_insert(Key, #hash_set{capacity = Capacity} = Set) ->
    NewCap = Capacity * 2,
    ResizedSet = new(NewCap),
    ResisedSetUpdated = foldl_set_helper(ResizedSet, Set, 0),
    insert(Key, ResisedSetUpdated).

resize_and_insertn(Key, Amount, #hash_set{capacity = Capacity} = Set) ->
    NewCap = Capacity * 2,
    ResizedSet = new(NewCap),
    ResisedSetUpdated = foldl_set_helper(ResizedSet, Set, 0),
    insertn(Key, Amount, ResisedSetUpdated).

foldl_set_helper(#hash_set{capacity = Capacity1, table = Table1} = Set1,
                 #hash_set{} = Set2,
                 I) ->
    if I == Capacity1 ->
           Key = get_element(I, Table1),
           Qua = get_elementq(I, Table1),
           if Key /= undefined ->
                  foldl_set_helper(Set1, insertn(Key, Qua, Set2), I + 2)
           end;
       true ->
           Set2
    end.

remove(Key,
       #hash_set{size = Size,
                 capacity = Capacity,
                 table = Table} =
           Set) ->
    Index = hash(Key, Capacity),
    Qua = get_elementq(Index, Table),
    case Qua of
        0 ->
            Set;
        1 ->
            NewTable1 = array:set(Index * 2, undefined, Table),
            NewTable2 = array:set(Index * 2 + 1, 0, NewTable1),
            %Tmp = print_arr(NewTable2, 0, Capacity*2),
            Set#hash_set{size = Size - 1,
                         capacity = Capacity,
                         table = NewTable2};
        _ ->
            NewTable1 = array:set(Index * 2 + 1, Qua - 1, Table),
            %Tmp = print_arr(NewTable1, 0, Capacity*2),
            Set#hash_set{size = Size,
                         capacity = Capacity,
                         table = NewTable1}
    end.

removen(Key,
        Amount,
        #hash_set{size = Size,
                  capacity = Capacity,
                  table = Table} =
            Set) ->
    Index = hash(Key, Capacity),
    Qua = get_elementq(Index, Table),
    Tval = Qua - Amount,
    case Tval of
        0 ->
            NewTable1 = array:set(Index * 2, undefined, Table),
            NewTable2 = array:set(Index * 2 + 1, 0, NewTable1),
            %Tmp = print_arr(NewTable2, 0, Capacity*2),
            Set#hash_set{size = Size - 1,
                         capacity = Capacity,
                         table = NewTable2};
        _ when Tval =< -1 ->
            Set;
        _ ->
            NewTable1 = array:set(Index * 2 + 1, Qua - 1, Table),
            %Tmp = print_arr(NewTable1, 0, Capacity*2),
            Set#hash_set{size = Size,
                         capacity = Capacity,
                         table = NewTable1}
    end.

has_key(#hash_set{capacity = Capacity, table = Table}, Key) ->
    Index = hash(Key, Capacity),
    Kval = get_element(Index, Table),
    if Kval == Key ->
           true;
       true ->
           false
    end.

amount_key(#hash_set{capacity = Capacity, table = Table}, Key) ->
    get_elementq(hash(Key, Capacity), Table).

to_list(#hash_set{} = Set) ->
    lists:sort(to_list(Set, 0, [])).

to_list(#hash_set{capacity = Capacity, table = Table} = Set, I, Lst) ->
    if I < Capacity ->
           Curel = get_element(I, Table),
           if Curel == undefined ->
                  to_list(Set, I + 1, Lst);
              true ->
                  to_list(Set, I + 1, add_to_list(Lst, Curel, get_elementq(I, Table)))
           end;
       true ->
           Lst
    end.

from_list(Lst) ->
    from_list(new(16), lists:sort(Lst)).

from_list(Set, []) ->
    Set;
from_list(#hash_set{} = Set, [Hi | Ti]) ->
    from_list(insert(Hi, Set), Ti).

add_to_list(Lst, _, 0) ->
    Lst;
add_to_list(Lst, Elem, N) ->
    add_to_list(lists:append(Lst, [Elem]), Elem, N - 1).

%print_arr(_, _, 0) -> io:fwrite("\n"), 0;
%print_arr(Arr, I, Lim) ->
%Val = array:get(I, Arr),
%if Val == undefined ->
%io:fwrite("U, "),
%print_arr(Arr, I+1, Lim-1);
%true ->
%io:fwrite(integer_to_list(Val)), io:fwrite(", "), print_arr(Arr, I+1, Lim-1)
%end.
%print_set(#hash_set{size = Size, capacity = Capacity, table = Table} = Set) ->
%print_arr(Table, 0, Capacity*2).

% filter(Pred, #hash_set{} = Set) ->
%     Lst = to_list(Set),
%     FilteredList = helper_filter_count(Pred, Lst, 0),
%     from_list(filterhelp(FilteredList)).

% helper_filter_count(_Fun, [], Count) ->
%     {[], Count};
% helper_filter_count(Fun, [undefined | T], Count) ->
%     {_, NewCount} = helper_filter_count(Fun, T, Count),
%     NewCount;
% helper_filter_count(Fun, [H | T], Count) ->
%     case Fun(H) of
%         true ->
%             {FilteredTale, NewCount} = helper_filter_count(Fun, T, Count + 1),
%             {[H | FilteredTale], NewCount};
%         false ->
%             {FilteredTale, NewCount} = helper_filter_count(Fun, T, Count),
%             {[undefined | FilteredTale], NewCount}
%     end.

% filterhelp({Lst, _}) ->
%     lists:nthtail(1, Lst).

filter(Pred, #hash_set{capacity = Capacity, table = Table} = _) ->
    NewSet = new(10),
    filter_lower(NewSet, Table, 0, Capacity * 2, Pred).

filter_lower(#hash_set{} = NewSet, Array, I, Lim, Pred) ->
    case I of
        Lim ->
            NewSet;
        _ ->
            filter_lower_lower(NewSet, Array, I, Lim, Pred)
    end.

filter_lower_lower(#hash_set{} = NewSet, Array, I, Lim, Pred) ->
    Elem = array:get(I, Array),
    Quan = array:get(I + 1, Array),
    case Elem of
        undefined ->
            filter_lower(NewSet, Array, I + 2, Lim, Pred);
        _ ->
            filter_lower(filter_lower_lower_lower(NewSet, Elem, Quan, Pred),
                         Array,
                         I + 2,
                         Lim,
                         Pred)
    end.

filter_lower_lower_lower(#hash_set{} = NewSet, Elem, Quan, Pred) ->
    Result = Pred(Elem),
    case Result of
        true ->
            insertn(Elem, Quan, NewSet);
        false ->
            NewSet
    end.

map(Fun, #hash_set{capacity = Capacity, table = Table} = _) ->
    NewSet = new(Capacity),
    map_lower(NewSet, Table, 0, Capacity * 2, Fun).

map_lower(#hash_set{} = NewSet, Array, I, Lim, Fun) ->
    case I of
        Lim ->
            NewSet;
        _ ->
            map_lower_lower(NewSet, Array, I, Lim, Fun)
    end.

map_lower_lower(#hash_set{} = NewSet, Array, I, Lim, Fun) ->
    Elem = array:get(I, Array),
    Quan = array:get(I + 1, Array),
    case Elem of
        undefined ->
            map_lower(NewSet, Array, I + 2, Lim, Fun);
        _ ->
            map_lower(insertn(Fun(Elem), Quan, NewSet), Array, I + 2, Lim, Fun)
    end.

foldl(#hash_set{capacity = Capacity, table = Table} = _, Fun, Acc) ->
    foldl(Table, 0, Capacity * 2, Fun, Acc).

foldl(Array, I, Lim, Fun, Acc) ->
    case I of
        Lim ->
            Acc;
        _ ->
            foldl_lower(Array, I, Lim, Fun, Acc)
    end.

foldl_lower(Array, I, Lim, Fun, Acc) ->
    % io:fwrite(integer_to_list(I)),
    % io:fwrite(" "),
    % io:fwrite(integer_to_list(Lim)),
    % io:fwrite("\n"),
    Elem = array:get(I, Array),
    case Elem of
        undefined ->
            foldl(Array, I + 2, Lim, Fun, Acc);
        _ ->
            NewAcc = foldl_lower_lower(Elem, array:get(I + 1, Array), Fun, Acc),
            foldl(Array, I + 2, Lim, Fun, NewAcc)
    end.

foldl_lower_lower(Elem, I, Fun, Acc) ->
    case I of
        0 ->
            Acc;
        _ ->
            NewAcc = Fun(Elem, Acc),
            foldl_lower_lower(Elem, I - 1, Fun, NewAcc)
    end.

foldr(#hash_set{capacity = Capacity, table = Table} = _, Fun, Acc) ->
    foldr(Table, Capacity * 2 - 2, 0, Fun, Acc).

foldr(Array, I, Lim, Fun, Acc) ->
    case I of
        Lim ->
            Acc;
        _ ->
            foldr_lower(Array, I, Lim, Fun, Acc)
    end.

foldr_lower(Array, I, Lim, Fun, Acc) ->
    Elem = array:get(I, Array),
    case Elem of
        undefined ->
            foldr(Array, I - 2, Lim, Fun, Acc);
        _ ->
            NewAcc = foldr_lower_lower(Elem, array:get(I + 1, Array), Fun, Acc),
            foldr(Array, I - 2, Lim, Fun, NewAcc)
    end.

foldr_lower_lower(Elem, I, Fun, Acc) ->
    case I of
        0 ->
            Acc;
        _ ->
            NewAcc = Fun(Elem, Acc),
            foldr_lower_lower(Elem, I - 1, Fun, NewAcc)
    end.

% foldl(#hash_set{} = Set, Fun, Acc) ->
%     Lst = to_list(Set),
%     lists:foldl(Fun, Acc, Lst).

% foldr(#hash_set{} = Set, Fun, Acc) ->
%     Lst = to_list(Set),
%     lists:foldr(Fun, Acc, Lst).

union(#hash_set{size = Size1, capacity = Capacity1} = Set1,
      #hash_set{size = Size2, capacity = Capacity2} = Set2) ->
    CrossKeyCnt =
        foldl(fun(Key, Acc) ->
                 case has_key(Set2, Key) of
                     true ->
                         Acc + 1;
                     false ->
                         Acc
                 end
              end,
              0,
              Set1),

    NewCap = max(max(Capacity1, Capacity2), Size1 + Size2 - CrossKeyCnt),
    NewSet = new(NewCap),
    SetWithSet1 = foldl_set_helper(NewSet, Set1, 0),
    foldl_set_helper(SetWithSet1, Set2, 0).

% main() ->
%     Lst = [1, 2, 3, 4, 5, 6, 4, 5, 5],
%     Set = hash:from_list(Lst),
%     Val = hash:foldl(Set, fun(X, Sum) -> X + Sum end, 0).
%     Set0 = hash:new(),
%     Set1 = hash:insert(3, Set0),
%     Set2 = hash:insert(7, Set1),
%     Set3 = hash:insert(22, Set2),
%     Set4 = hash:insert(15, Set3),
%     Set5 = hash:map(fun(X) -> X * X end, Set4),
%     hash:has_key(Set5, 9).
%     Lst = [1,2,3,4,4,5,5,5,6],
%     Set = hash:from_list(Lst),
%     LstNew = hash:to_list(Set),
%     Answ = LstNew =:= Lst.
% Lst = [1, 2, 3, 4, 5, 6, 4, 5, 5],
%     Set = hash:from_list(Lst),
%     LstNew = hash:to_list(Set),
%     %Answ = LstNew =:= Lst.
%    Lst.
%     Set0 = hash:new(),
%     Set1 = hash:insert(9, Set0),
%     Set2 = hash:insert(8, Set1),
%     Set3 = hash:insert(7, Set2),
%     Set4 = hash:insert(10, Set3),
%     Set5 = hash:filter(fun(X) -> X > 7 end, Set4).

   % io:fwrite(integer_to_list(Set5#hash_set.size)).

% Set = new(16),
% Set1 = insert(5, Set),
% Set2 = insert(5, Set1),
% Set3 = insertn(6, 10, Set2),
% to_list(Set3).

% Lst = [1, 2, 3, 4, 5, 6, 4, 5, 5],
% Set = from_list(Lst),
% foldl(Set, fun(X, Sum) -> X + Sum end, 0).
