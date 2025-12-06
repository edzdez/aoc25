open! Core

type op = Plus | Times

let parse_nums input =
  List.map input ~f:(fun line ->
      String.split_on_chars line ~on:[ ' ' ]
      |> List.filter_map ~f:(function
        | "" -> None
        | s -> Some (int_of_string s)))

let parse_ops input =
  String.split_on_chars input ~on:[ ' ' ]
  |> List.filter_map ~f:(function
    | "+" -> Some Plus
    | "*" -> Some Times
    | _ -> None)

let p1 input =
  let lines = String.split_lines input in
  let nums, ops = List.split_n lines (List.length lines - 1) in
  let nums, ops =
    (List.transpose_exn @@ parse_nums nums, parse_ops @@ List.hd_exn ops)
  in
  List.zip_exn nums ops
  |> List.fold ~init:0 ~f:(fun acc (nums, op) ->
      acc
      +
      match op with
      | Plus -> List.fold nums ~init:0 ~f:( + )
      | Times -> List.fold nums ~init:1 ~f:( * ))

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d6p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 4277556 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d6p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 6 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p1 input);
  [%expect {| 5361735137219 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p2 input);
  [%expect {| 7101 |}]
*)
