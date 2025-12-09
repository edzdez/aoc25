open! Core

type point = int * int

let point_of_int s =
  match String.split_on_chars s ~on:[ ',' ] with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> failwith "invalid point"

let rec get_pairs ?(seen = []) ?(acc = []) = function
  | [] -> acc
  | hd :: tl ->
      let acc = List.fold seen ~init:acc ~f:(fun acc y -> (y, hd) :: acc) in
      get_pairs ~seen:(hd :: seen) ~acc tl

let rect_size (x1, y1) (x2, y2) = abs (x1 - x2 + 1) * abs (y1 - y2 + 1)

let p1 input =
  let points = String.split_lines input |> List.map ~f:point_of_int in
  let pairs = get_pairs points in
  List.fold pairs ~init:0 ~f:(fun best (a, b) -> Int.max best @@ rect_size a b)

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d9p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 50 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d9p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 40 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d9.in" in
  printf "%d" (p1 input);
  [%expect {| 4749672288 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d9.in" in
  printf "%d" (p2 input);
  [%expect {| 17921968177009 |}]
*)
