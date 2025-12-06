open! Core

let p1 _input = assert false
let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d6p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 3 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d6p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 6 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p1 input);
  [%expect {| 1097 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p2 input);
  [%expect {| 7101 |}]
*)
