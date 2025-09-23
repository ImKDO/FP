(* Преобразуем число в список цифр *)
let digits_of_z n =
  let rec aux m acc =
    if Z.equal m Z.zero then acc
    else
      let q, r = Z.ediv_rem m (Z.of_int 10) in
      aux q (Z.to_int r :: acc)
  in
  aux n []

(* Сумма цифр большого числа через List.iter *)
let sum_digits n =
  let s = ref 0 in
  let digits = digits_of_z n in
  List.iter (fun d -> s := Stdlib.( + ) !s d) digits;
  !s

let ans =
  sum_digits (Z.pow (Z.of_int 2) 1000)
