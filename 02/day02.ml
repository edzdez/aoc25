open! Core

type interval = int64 * int64

let interval_of_string s : interval =
  let l, r = String.lsplit2_exn s ~on:'-' in
  (Int64.of_string l, Int64.of_string r)

let in_interval (a, b) ~test = Int64.(a <= test && test <= b)

let invalid_ids ~digits : Int64.t list =
  if digits mod 2 <> 0 then []
  else
    let pat_digits = digits / 2 in
    let begin_ = Int.pow 10 (pat_digits - 1) in
    let size = 9 * begin_ in
    let pats = List.init size ~f:(fun x -> string_of_int @@ (begin_ + x)) in
    List.map pats ~f:(fun pat -> Int64.of_string @@ pat ^ pat)

let p1 input =
  let intervals =
    String.split input ~on:',' |> List.map ~f:interval_of_string
  in
  List.init 5 ~f:(fun x -> 2 * (x + 1))
  |> List.concat_map ~f:(fun digits -> invalid_ids ~digits)
  |> List.filter ~f:(fun test -> List.exists intervals ~f:(in_interval ~test))
  |> List.fold ~init:Int64.zero ~f:Int64.( + )

let p2 _input = assert false

let run ~part input =
  let input = String.strip ~drop:Char.is_whitespace input in
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input =
    In_channel.read_all "../inputs/d2p1.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p1 input);
  [%expect {| 1227775554 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d2p1.sample" in
  printf "%d" (p2 input);
  [%expect {| 6 |}]
*)

let%expect_test "part 1" =
  let input =
    In_channel.read_all "../inputs/d2.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p1 input);
  [%expect {| 28846518423 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d2.in" in
  printf "%d" (p2 input);
  [%expect {| 7101 |}]
*)
