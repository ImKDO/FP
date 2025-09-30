(* Функция для вычисления длины последовательности Коллатца *)
let collatz_length n =
  let count = ref 0 in
  let current = ref n in
  while !current <> 1 do
    if !current mod 2 = 0 then current := !current / 2
    else current := (3 * !current) + 1;
    incr count
  done;
  !count + 1

let find_max_collatz limit =
  let max_length = ref 0 in
  let max_number = ref 1 in

  for i = 1 to limit do
    let length = collatz_length i in
    if length > !max_length then (
      max_length := length;
      max_number := i)
  done;

  (!max_number, !max_length)

(* Основное вычисление *)
let ans = fst (find_max_collatz 1_000_000)
