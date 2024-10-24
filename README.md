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
```
Свертки (левая и правая):
```erlang
foldl(#hash_set{} = Set, Fun, Acc) ->
    Lst = to_list(Set),
    lists:foldl(Fun, Acc, Lst).

foldr(#hash_set{} = Set, Fun, Acc) ->
    Lst = to_list(Set),
    lists:foldr(Fun, Acc, Lst).
```

Отображение (map):
```erlang
map(Fun, #hash_set{} = Set) ->
    Lst = to_list(Set),
    NewLst = lists:map(Fun, Lst),
    from_list(NewLst).
```
Фильтрация (filter):
```erlang
filter(Pred, #hash_set{} = Set) ->
    Lst = to_list(Set),
    FilteredList = helper_filter_count(Pred, Lst, 0),
    from_list(filterhelp(FilteredList)).

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
