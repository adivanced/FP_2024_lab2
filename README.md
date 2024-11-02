# Лабораторная работа №2 (OpenAddress HashMap MultiSet)

---

* Студент: `Панин Иван Михайлович`
* Группа: `P34082`
* ИСУ: `369405`
* Функциональный язык: `Erlang`

--- 

## Требования

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

--- 

## Ключевые элементы реализации

Добавление и удаление элементов:
```erlang
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
            Set#hash_set{size = newsize(Size, A),
                         capacity = Capacity,
                         table = NewTable2}
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
```
Свертки (левая и правая):
```erlang
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
```

Отображение (map):
```erlang
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
```
Фильтрация (filter):
```erlang
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

```

## Соответствие свойству моноида
- Нейтральным элементом будет являться пустой `HashSet` - new()

- Определил бинарную операцию union:
```erlang
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
```

## Тестирование

В данной лабораторной работе я использовал два инструмента для тестирования:

- EUnit - для модульного тестирования
- Proper - для тестирования свойств (property-based)

---

## Выводы

В результате выполнения данной лабораторной работы я ближе познакомился 
с языком Erlang, также реализовал структуру OpenAddress HashMap MultiSet, протестировал корректность структуры с помощью модульных тестов и тестов свойств предикатов.
