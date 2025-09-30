let test_1 () = Alcotest.(check int) "Task 1 Ans: 1366" 1366 Lab1.Task_1.ans
let test_2 () = Alcotest.(check int) "Task 2 Ans: 1366" 1366 Lab1.Task_2.ans
let test_3 () = Alcotest.(check int) "Task 3 Ans: 1366" 1366 Lab1.Task_3.ans
let test_4 () = Alcotest.(check int) "Task 4 Ans: 1366" 1366 Lab1.Task_4.ans
let test_5 () = Alcotest.(check int) "Task 5 Ans: 1366" 1366 Lab1.Task_5.ans

let tasks_test =
  [
    ("Task 1", `Quick, test_1);
    ("Task 2", `Quick, test_2);
    ("Task 3", `Quick, test_3);
    ("Task 4", `Quick, test_4);
    ("Task 5", `Quick, test_5);
  ]

let () = Alcotest.run "Lab1 Task14 tests" [ ("Task16", tasks_test) ]
