open! Core

type interval = string * string

let interval_of_string s =
  let l, r = String.lsplit2_exn s ~on:'-' in
  (String.pad_left ~char:'0' ~len:15 l, String.pad_left ~char:'0' ~len:15 r)

let parse_db = List.map ~f:interval_of_string

let p1 input =
  let db_lines, query_lines =
    String.split_lines input
    |> List.split_while ~f:(Fn.compose not String.is_empty)
  in
  let db = parse_db db_lines in
  List.count query_lines ~f:(fun query ->
      let query = String.pad_left ~char:'0' ~len:15 query in
      List.exists db ~f:(fun (a, b) -> String.(a <= query && query <= b)))

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d5p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 3 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d5p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 43 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d5.in" in
  printf "%d" (p1 input);
  [%expect {| 505 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d5.in" in
  printf "%d" (p2 input);
  [%expect {| 8713 |}]
*)
