let result exp =
  let rec power = function
    | 0 -> [ 1 ]
    | n ->
        let prev = power (n - 1) in
        let rec multiply_by_two lst carry acc =
          match lst with
          | [] -> if carry = 0 then List.rev acc else List.rev (carry :: acc)
          | x :: xs ->
              let prod = (x * 2) + carry in
              multiply_by_two xs (prod / 10) ((prod mod 10) :: acc)
        in
        multiply_by_two prev 0 []
  in
  let result = power exp in
  let rec sum_digits lst acc =
    match lst with [] -> acc | x :: xs -> sum_digits xs (acc + x)
  in
  sum_digits result 0

let ans = result 1000
