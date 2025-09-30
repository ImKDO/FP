let rec collatz n =
  if n = 1 then 1
  else if n mod 2 = 0 then 1 + collatz (n / 2)
  else 1 + collatz ((n * 3) + 1)

let rec find_max_number_collatz n max_len max_number =
  if n <= 1 then max_number
  else
    let len = collatz n in
    if max_len < len then find_max_number_collatz (n - 1) len n
    else find_max_number_collatz (n - 1) max_len max_number

let ans = find_max_number_collatz 1_000_000 1 1
