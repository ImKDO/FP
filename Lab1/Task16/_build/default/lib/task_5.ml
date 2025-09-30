let digits_of_z n =
  let rec aux m () =
    if Z.equal m Z.zero then Seq.Nil
    else
      let q, r = Z.ediv_rem m (Z.of_int 10) in
      Seq.Cons (Z.to_int r, aux q)
  in
  aux n

let sum_digits n = digits_of_z n |> Seq.fold_left Stdlib.( + ) 0
let ans = sum_digits (Z.pow (Z.of_int 2) 1000)
