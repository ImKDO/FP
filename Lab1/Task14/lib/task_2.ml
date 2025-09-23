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