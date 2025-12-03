open! Core

type bank = int list

let parse_banks input =
  String.split_lines input
  |> List.map ~f:(fun line ->
      String.to_list line |> List.map ~f:Char.get_digit_exn)

let get_max ~min_size bank =
  match
    List.drop (List.rev bank) min_size |> List.max_elt ~compare:Int.compare
  with
  | Some x -> x
  | None -> assert false

let max_joltage ~digits bank =
  (* we can be greedy *)
  let rec aux digits bank =
    if digits = 0 then 0L
    else
      let bat = get_max ~min_size:(digits - 1) bank in
      let rest = List.drop (List.drop_while bank ~f:(fun x -> x <> bat)) 1 in
      let res = aux (digits - 1) rest in
      Int64.((of_int bat * pow 10L (of_int digits - 1L)) + res)
  in
  aux digits bank

let p1 input =
  let banks = parse_banks input in
  let open Int64 in
  List.fold banks ~init:0L ~f:(fun acc bank -> acc + max_joltage ~digits:2 bank)

let p2 input =
  let banks = parse_banks input in
  let open Int64 in
  List.fold banks ~init:0L ~f:(fun acc bank ->
      acc + max_joltage ~digits:12 bank)

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
  printf "%Ld" (p1 input);
  [%expect {| 357 |}]

let%expect_test "part 2 sample input" =
  let input =
    In_channel.read_all "../inputs/d3p2.sample"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p2 input);
  [%expect {| 3121910778619 |}]

let%expect_test "part 1" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p1 input);
  [%expect {| 17278 |}]

let%expect_test "part 2" =
  let input =
    In_channel.read_all "../inputs/d3.in"
    |> String.strip ~drop:Char.is_whitespace
  in
  printf "%Ld" (p2 input);
  [%expect {| 171528556468625 |}]
