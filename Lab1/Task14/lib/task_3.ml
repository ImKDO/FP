let rec collatz n acc =
  if n = 1 then acc
  else if n mod 2 = 0 then collatz (n / 2) (acc + 1)
  else collatz ((n * 3) + 1) (acc + 1)

let collatz_for_seq x = collatz x 1
let n = 1_000_000
let num_seq = List.init n (fun x -> x + 1)
let lst_ans = List.map collatz_for_seq num_seq

let rec max_ans lst max_num max_len index =
  match lst with
  | [] -> max_num
  | h :: t ->
      max_ans t
        (if max_len < h then index else max_num)
        (if max_len < h then h else max_len)
        (index + 1)

let ans = max_ans lst_ans (-1) 1 1
