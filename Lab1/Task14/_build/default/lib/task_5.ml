let rec collatz_seq n =
  if n = 1 then fun () -> Seq.Cons (1, Seq.empty)
  else fun () ->
    let next = if n mod 2 = 0 then n / 2 else (3 * n) + 1 in
    Seq.Cons (n, collatz_seq next)

let collatz_length n = collatz_seq n |> Seq.fold_left (fun acc _ -> acc + 1) 0
let collatz_pairs = Seq.ints 1 |> Seq.map (fun n -> (n, collatz_length n))

let find_max_collatz limit =
  collatz_pairs |> Seq.take limit
  |> Seq.fold_left
       (fun (max_n, max_len) (n, len) ->
         if len > max_len then (n, len) else (max_n, max_len))
       (1, 1)

let ans = fst (find_max_collatz 1_000_000)
