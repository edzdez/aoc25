open! Core

type interval = int64 * int64

let interval_of_string s : interval =
  let l, r = String.lsplit2_exn s ~on:'-' in
  (Int64.of_string l, Int64.of_string r)

let in_interval (a, b) ~test = Int64.(a <= test && test <= b)
let length (a, b) = Int.of_int64_exn Int64.(b - a)

let pats digits =
  let begin_ = Int.pow 10 (digits - 1) in
  let size = 9 * begin_ in
  List.init size ~f:(fun x -> string_of_int @@ (begin_ + x))

let invalid_ids digits : Int64.t list =
  if digits mod 2 <> 0 then []
  else
    let digits = digits / 2 in
    let pats = pats digits in
    List.map pats ~f:(fun pat -> Int64.of_string @@ pat ^ pat)

let rec is_repeated ~pat ~chunk x =
  let open Int64 in
  if x = pat then true
  else if rem x chunk <> pat then false
  else is_repeated ~pat ~chunk (x / chunk)

let is_invalid x =
  let as_string = Int64.to_string x in
  let digits = String.length @@ as_string in
  let rec aux d =
    if d = 0 then false
    else if digits mod d <> 0 then aux (d - 1)
    else
      let pat = Int64.of_string @@ String.prefix as_string d in
      let chunk = Int64.of_int @@ Int.pow 10 d in
      if is_repeated ~pat ~chunk x then true else aux (d - 1)
  in
  aux (digits / 2)

let p1 input =
  let intervals =
    String.split input ~on:',' |> List.map ~f:interval_of_string
  in
  List.init 5 ~f:(fun x -> 2 * (x + 1))
  |> List.concat_map ~f:invalid_ids
  |> List.filter ~f:(fun test -> List.exists intervals ~f:(in_interval ~test))
  |> List.fold ~init:Int64.zero ~f:Int64.( + )

let p2 input =
  let intervals =
    String.split input ~on:',' |> List.map ~f:interval_of_string
  in
  let open Int64 in
  List.fold intervals ~init:zero ~f:(fun cnt ((a, _) as interval) ->
      cnt
      + (List.init (length interval) ~f:(fun x -> a + of_int x)
        |> List.fold ~init:zero ~f:(fun cnt x ->
            if is_invalid x then cnt + x else cnt)))

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

let%expect_test "part 2 sample input" =
  let input =
    In_channel.read_all "../inputs/d2p2.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p2 input);
  [%expect {| 4174379243 |}]

let%expect_test "part 1" =
  let input =
    In_channel.read_all "../inputs/d2.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p1 input);
  [%expect {| 28846518423 |}]

let%expect_test "part 2" =
  let input =
    In_channel.read_all "../inputs/d2.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p2 input);
  [%expect {| 31578210022 |}]
