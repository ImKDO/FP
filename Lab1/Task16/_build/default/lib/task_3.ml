let sum_digits n =
  let s = Z.to_string n in
  s
  |> String.to_seq
  |> Seq.map (fun ch -> int_of_char ch - int_of_char '0')
  |> Seq.fold_left Stdlib.( + ) 0

let ans =
  sum_digits (Z.pow (Z.of_int 2) 1000)
