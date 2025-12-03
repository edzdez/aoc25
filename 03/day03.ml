open! Core

type bank = int list

let max_joltage bank =
  (* we can be greedy *)
  let aux =
    let open Option.Let_syntax in
    let%bind first =
      List.max_elt ~compare:Int.compare @@ List.drop_last_exn bank
    in
    let%bind second =
      List.max_elt ~compare:Int.compare
      @@ List.drop (List.drop_while bank ~f:(fun x -> x <> first)) 1
    in
    return ((first * 10) + second)
  in
  match aux with Some n -> n | None -> assert false

let p1 input =
  let banks =
    String.split_lines input
    |> List.map ~f:(fun line ->
        String.to_list line |> List.map ~f:Char.get_digit_exn)
  in
  List.fold banks ~init:0 ~f:(fun acc bank -> acc + max_joltage bank)

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
  [%expect {| 357 |}]

(*
let%expect_test "part 2 sample input" =
  let input =
    In_channel.read_all "../inputs/d3p2.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p2 input);
  [%expect {| 4174379243 |}]
*)

let%expect_test "part 1" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p1 input);
  [%expect {| 17278 |}]

(*
let%expect_test "part 2" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%d" (p2 input);
  [%expect {| 31578210022 |}]
*)
