module Digits = struct
  let of_z (n : Z.t) : int Seq.t =
    Z.to_string n
    |> String.to_seq
    |> Seq.map (fun ch -> int_of_char ch - int_of_char '0')
end

(* Модуль фильтрации *)
module Filter = struct
  let positive seq =
    Seq.filter (fun d -> d > 0) seq
end

(* Модуль свёртки *)
module Reduce = struct
  let sum seq =
    Seq.fold_left Stdlib.( + ) 0 seq
end

(* Итоговое решение *)
let ans =
  let n = Z.pow (Z.of_int 2) 1000 in
  n
  |> Digits.of_z     (* генерация *)
  |> Filter.positive (* фильтрация *)
  |> Reduce.sum      (* свёртка *)
