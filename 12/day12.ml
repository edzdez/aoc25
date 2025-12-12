open! Core

let p1 _input = assert false
let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d12p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 5 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d12p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 2 |}]
*)

(*
let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d12.in" in
  printf "%d" (p1 input);
  [%expect {| 668 |}]
*)

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d12.in" in
  printf "%d" (p2 input);
  [%expect {| 294310962265680 |}]
*)
