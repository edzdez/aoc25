open! Core

type interval = Int64.t * Int64.t
type extent = Begin of Int64.t | End of Int64.t

let extract_extent = function Begin s -> s | End s -> s

let interval_of_string s =
  let l, r = String.lsplit2_exn s ~on:'-' in
  (Int64.of_string l, Int64.of_string r)

let parse_input input =
  let db_lines, query_lines =
    String.split_lines input
    |> List.split_while ~f:(Fn.compose not String.is_empty)
  in
  (List.map ~f:interval_of_string db_lines, List.drop query_lines 1)

let p1 input =
  let db, queries = parse_input input in
  Int64.of_int
  @@ List.count queries ~f:(fun query ->
      let open Int64 in
      let query = of_string query in
      List.exists db ~f:(fun (a, b) -> a <= query && query <= b))

let p2 input =
  let events =
    parse_input input |> fst
    |> List.concat_map ~f:(fun (l, r) -> [ Begin l; End Int64.(r + 1L) ])
    |> List.sort ~compare:(fun l r ->
        Int64.compare (extract_extent l) (extract_extent r))
  in
  let _, cnt =
    List.fold events ~init:(None, 0L) ~f:(fun (start, cnt) extent ->
        match (start, extent) with
        | None, Begin l -> (Some (l, 1), cnt)
        | Some (l, nest), Begin _ -> (Some (l, nest + 1), cnt)
        | Some (l, 1), End r -> (None, Int64.(cnt + r - l))
        | Some (l, nest), End _ -> (Some (l, nest - 1), cnt)
        | _ -> assert false)
  in
  cnt

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d5p1.sample" in
  printf "%Ld" (p1 input);
  [%expect {| 3 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d5p2.sample" in
  printf "%Ld" (p2 input);
  [%expect {| 14 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d5.in" in
  printf "%Ld" (p1 input);
  [%expect {| 505 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d5.in" in
  printf "%Ld" (p2 input);
  [%expect {| 344423158480189 |}]
