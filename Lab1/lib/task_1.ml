module Task1 = struct
  let rec collatz n acc =
  if n == 1 then acc 
  else if n mod 2 = 0 then collatz(n/2)(acc + 1)
  else collatz(n*3 + 1)(acc + 1)


let rec find_max_number_collatz n max_len max_number =
  if n <= 1 then max_number
  else let len = collatz(n)(1) in
  if max_len < len then find_max_number_collatz(n - 1)(len)(n)
  else find_max_number_collatz(n - 1)(max_len)(max_number)

end