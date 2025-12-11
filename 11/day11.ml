open! Core

let p1 _input = assert false
let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d11p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 50 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d11p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 24 |}]
*)

(*
let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p1 input);
  [%expect {| 4749672288 |}]
*)

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p2 input);
  [%expect {| 1479665889 |}]
*)
