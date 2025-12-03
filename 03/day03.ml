open! Core

let p1 _input = assert false
let p2 _input = assert false

let run ~part input =
  let input = String.strip ~drop:Char.is_whitespace input in
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input =
    In_channel.read_all "../inputs/d3p1.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p1 input);
  [%expect {| 1227775554 |}]

(*
let%expect_test "part 2 sample input" =
  let input =
    In_channel.read_all "../inputs/d3p2.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p2 input);
  [%expect {| 4174379243 |}]

let%expect_test "part 1" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p1 input);
  [%expect {| 28846518423 |}]

let%expect_test "part 2" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p2 input);
  [%expect {| 31578210022 |}]
*)
