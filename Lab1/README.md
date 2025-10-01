# Лабораторная работа №1. Функциональное программирование

**Студент:** Кравченко Дмитрий Олегович  
**Группа:** P3319

---

## Описание проблемы

**Problem 14: Longest Collatz Sequence**

Задача из проекта Эйлера: найти число меньше одного миллиона, которое производит самую длинную последовательность Коллатца.

> Следующая итеративная последовательность определена для множества положительных целых чисел:
>
> n → n/2 (n четное)  
> n → 3n + 1 (n нечетное)
>
> Используя правило выше и начиная с 13, мы получаем следующую последовательность:  
> 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1.
>
> Можно увидеть, что эта последовательность (начинающаяся с 13 и заканчивающаяся на 1) содержит 10 элементов.
>
> Какое начальное число меньше одного миллиона производит самую длинную цепочку?

**Правильный ответ:** 837799

---

## Ключевые элементы реализации

### 1. Монолитная реализация с хвостовой рекурсией (task_1.ml)

```ocaml
let rec collatz n =
if n = 1 then 1
else if n mod 2 = 0 then 1 + collatz(n/2)
else 1 + collatz(n*3 + 1)


let rec find_max_number_collatz n max_len max_number =
if n <= 1 then max_number
else let len = collatz(n) in
if max_len < len then find_max_number_collatz(n - 1)(len)(n)
else find_max_number_collatz(n - 1)(max_len)(max_number)

let ans = find_max_number_collatz 1_000_000 1 1
```

### 2. Модульная реализация с fold/filter (task_2.ml)

```ocaml
module GenerateSeq = struct
  let first_n_natural_number n = List.init n (fun i -> i + 1)
end

module FilteredUintSeq = struct
  let filtered_seq_even (lst: int list) = List.filter (fun x -> x mod 2 = 0) lst
  let filtered_seq_odd (lst: int list) = List.filter (fun x -> x mod 2 != 0) lst
end

module ReduceUintSeq = struct

let rec collatz n acc =
  if n == 1 then acc
  else if n mod 2 = 0 then collatz(n/2)(acc + 1)
  else collatz(n*3 + 1)(acc + 1)


  let collatz_seq lst =
  List.fold_left (fun (max_num, max_len) x ->
    let len = collatz x 1 in
    if len > max_len then (x,len)
    else (max_num, max_len)
    ) (1,0) lst
end

let num = 1_000_000

let first_n_num = GenerateSeq.first_n_natural_number num
let first_n_num_even = FilteredUintSeq.filtered_seq_even first_n_num
let first_n_num_odd = FilteredUintSeq.filtered_seq_odd first_n_num

let first_n_num_even_collatz = ReduceUintSeq.collatz_seq first_n_num_even
let first_n_num_odd_collatz = ReduceUintSeq.collatz_seq first_n_num_odd

let ans =
  if snd first_n_num_even_collatz > snd first_n_num_odd_collatz
  then fst first_n_num_even_collatz
  else fst first_n_num_odd_collatz
```

### 3. Реализация с отображением (task_3.ml)

```ocaml
let rec collatz n acc =
  if n = 1 then acc
  else if n mod 2 = 0 then collatz(n/2)(acc + 1)
  else collatz(n*3 + 1)(acc + 1)

let collatz_for_seq x = collatz x 1
let n = 1_000_000

let num_seq = List.init n (fun x -> x + 1)

let lst_ans = List.map collatz_for_seq num_seq
let rec max_ans lst max_num max_len index = match lst with
| [] -> max_num
| h :: t -> max_ans(t)(if max_len < h then index else max_num)(if max_len < h then h else max_len)(index + 1)

let ans = max_ans (lst_ans) (-1) (1) (1)
```

### 4. Реализация с циклами (task_4.ml)

```ocaml
let collatz_length n =
  let count = ref 0 in
  let current = ref n in
  while !current <> 1 do
    if !current mod 2 = 0 then
      current := !current / 2
    else
      current := 3 * !current + 1;
    incr count
  done;
  !count + 1

let find_max_collatz limit =
  let max_length = ref 0 in
  let max_number = ref 1 in

  for i = 1 to limit do
    let length = collatz_length i in
    if length > !max_length then begin
      max_length := length;
      max_number := i
    end
  done;

  (!max_number, !max_length)

let ans =
  fst (let limit = 1_000_000 in
  find_max_collatz limit)
```

### 5. Реализация с бесконечными последовательностями (task_5.ml)

```ocaml
let rec collatz_seq n =
  if n = 1 then
    fun () -> Seq.Cons (1, Seq.empty)
  else
    fun () ->
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      Seq.Cons (n, collatz_seq next)

let collatz_length n =
  collatz_seq n |> Seq.fold_left (fun acc _ -> acc + 1) 0

let collatz_pairs =
  Seq.ints 1 |> Seq.map (fun n -> (n, collatz_length n))

let find_max_collatz limit =
  collatz_pairs
  |> Seq.take limit
  |> Seq.fold_left (fun (max_n, max_len) (n, len) ->
      if len > max_len then (n, len) else (max_n, max_len)
    ) (1, 1)

let ans = fst (find_max_collatz 1_000_000)
```

### 6. Реализация на Python (alg.py)

```python
def collatz_sequence(n: int) -> tuple[int, int]:
    max_len: int = 0
    max_num: int = 0
    for i in range(1, n):
        count: int = 1
        temp: int = i
        while temp != 1:
            if temp % 2 == 0:
                temp //= 2
            else:
                temp = 3*temp + 1
            count += 1
        if count > max_len:
            max_num = i
            max_len = count
    return max_num, max_len
```

---

## Результаты выполнения

### OCaml реализации:

```
Result 1: 837799
Result 2: 837799
Result 3: 837799
Result 4: 837799
Result 5: 837799
```

### Python реализация:

```
(837799, 525)
```

---

## Выводы

### Анализ использованных приемов программирования

#### **1. Хвостовая рекурсия vs Обычная рекурсия**

- Использование аккумулятора предотвращает переполнение стека

#### **2. Модульность с разделением по типам чисел**

- Интересный подход: разделение на четные и нечетные числа
- Позволяет анализировать различные паттерны в последовательностях
- Демонстрирует композицию результатов из разных модулей
- `fold_left` эффективно обрабатывает большие списки

#### **3. Отображения и индексация**

- `List.map` естественно преобразует числа в длины последовательностей
- Необходимость отслеживания индексов усложняет код
- Показывает важность выбора правильной структуры данных

#### **4. Императивные конструкции**

#### **5. Ленивые последовательности**

- `Seq` в OCaml предоставляет элегантное решение
- Бесконечные потоки позволяют работать с любыми диапазонами
- Pipe операторы (`|>`) улучшают читаемость кода
- Демонстрируют мощь функционального подхода

## Описание проблемы

**Problem 16: Power Digit Sum**

Задача из проекта Эйлера: найти сумму цифр числа 2^1000.

> 2^15 = 32768 и сумма его цифр равна 3 + 2 + 7 + 6 + 8 = 26.  
> Какова сумма цифр числа 2^1000?

**Правильный ответ:** 1366

---

## Ключевые элементы реализации

### 1. Монолитная реализация с хвостовой рекурсией (task_1.ml)

```ocaml
let result exp =
  let rec power = function
    | 0 -> [1]
    | n ->
        let prev = power (n - 1) in
        let rec multiply_by_two lst carry acc =
          match lst with
          | [] -> if carry = 0 then List.rev acc else List.rev (carry :: acc)
          | x :: xs ->
              let prod = x * 2 + carry in
              multiply_by_two xs (prod / 10) ((prod mod 10) :: acc)
        in
        multiply_by_two prev 0 []
  in
  let result = power exp in
  let rec sum_digits lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> sum_digits xs (acc + x)
  in
  sum_digits result 0
let ans = result 1000
```

**Особенности:**

- Использует собственную реализацию больших чисел через списки
- Оптимизирована для предотвращения переполнения стека
- Накопитель передается как параметр

### 2. Монолитная реализация с обычной рекурсией (task_2.ml)

```ocaml
module Digits = struct
  let of_z (n : Z.t) : int Seq.t =
    Z.to_string n
    |> String.to_seq
    |> Seq.map (fun ch -> int_of_char ch - int_of_char '0')
end

module Filter = struct
  let positive seq =
    Seq.filter (fun d -> d > 0) seq
end

module Reduce = struct
  let sum seq =
    Seq.fold_left Stdlib.( + ) 0 seq
end

let ans =
  let n = Z.pow (Z.of_int 2) 1000 in
  n
  |> Digits.of_z
  |> Filter.positive
  |> Reduce.sum
```

**Особенности:**

- Классическая рекурсивная реализация
- Менее эффективна по памяти для больших значений
- Простота понимания и реализации

### 3. Модульная реализация с map (task_3.ml)

```ocaml
let sum_digits n =
  let s = Z.to_string n in
  s
  |> String.to_seq
  |> Seq.map (fun ch -> int_of_char ch - int_of_char '0')
  |> Seq.fold_left Stdlib.( + ) 0

let ans =
  sum_digits (Z.pow (Z.of_int 2) 1000)
```

**Особенности:**

- Четкое разделение ответственности между модулями
- Использование стандартных функций высшего порядка
- Легко тестируемые компоненты

### 4. Реализация с циклами

```ocaml
let digits_of_z n =
  let rec aux m acc =
    if Z.equal m Z.zero then acc
    else
      let q, r = Z.ediv_rem m (Z.of_int 10) in
      aux q (Z.to_int r :: acc)
  in
  aux n []

let sum_digits n =
  let s = ref 0 in
  let digits = digits_of_z n in
  List.iter (fun d -> s := Stdlib.( + ) !s d) digits;
  !s

let ans =
  sum_digits (Z.pow (Z.of_int 2) 1000)
```

**Особенности:**

- Использует map для преобразования последовательностей
- Демонстрирует функциональный подход к генерации данных
- Комбинирует map с fold для получения результата

### 5. Реализация с бесконечными циклами (task_5.ml)

```ocaml
let digits_of_z n =
  let rec aux m () =
    if Z.equal m Z.zero then Seq.Nil
    else
      let q, r = Z.ediv_rem m (Z.of_int 10) in
      Seq.Cons (Z.to_int r, aux q)
  in
  aux n

let sum_digits n =
  digits_of_z n |> Seq.fold_left Stdlib.( + ) 0

let ans =
  sum_digits (Z.pow (Z.of_int 2) 1000)
```

**Особенности:**

- Использует мутабельные ссылки (ref)
- Императивные конструкции for/while
- Ближе к традиционному программированию

### 6. Реализация на Python (alg.py)

```python
def power_digit_sum_functional(base, exp):
    return sum(map(int, str(base ** exp)))

def power_digit_sum_with_generators(base, exp):
    def digit_generator(number):
        for char in str(number):
            yield int(char)

    power_result = base ** exp
    digits = digit_generator(power_result)
    return sum(accumulating_sum_generator(digits))

def infinite_powers(base):
    power = 1
    while True:
        yield power
        power *= base
```

---

## Результаты выполнения

### OCaml реализации:

```
Result 1 (tail recursion): 1366
Result 2 (regular recursion): 1366
Result 3 (modular with fold/filter): 1366
Result 4 (with map): 1366
Result 5 (with loops): 1366
```

### Python реализации:

```
Result 6: 1366
```

---

## Выводы

### Анализ использованных приемов программирования

#### **1. Хвостовая рекурсия vs Обычная рекурсия**

- **Хвостовая рекурсия** эффективнее по памяти, особенно для больших вычислений
- **Обычная рекурсия** проще для понимания, но может вызвать переполнение стека

#### **2. Модульность и разделение ответственности**

- Модульный подход значительно улучшает читаемость кода
- Каждый модуль решает конкретную задачу (генерация, фильтрация, свертка)
- Легко тестировать и модифицировать отдельные компоненты

#### **3. Функциональные преобразования**

- `map` позволяет элегантно преобразовывать данные
- Комбинирование `map`, `fold` и `filter` дает мощные возможности обработки
- Функциональный стиль уменьшает количество ошибок

#### **4. Императивные конструкции в функциональном языке**

- OCaml поддерживает императивный стиль через `ref`, циклы
- Полезно для алгоритмов, естественно выражаемых циклами

---

