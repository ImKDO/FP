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
