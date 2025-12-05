open! Core

type interval = Int64.t * Int64.t
type extent = Begin of Int64.t | End of Int64.t

let extract_extent = function Begin s -> s | End s -> s

let interval_of_string s =
  let l, r = String.lsplit2_exn s ~on:'-' in
  (Int64.of_string l, Int64.of_string r)

let parse_db = List.map ~f:interval_of_string

let p1 input =
  let db_lines, query_lines =
    String.split_lines input
    |> List.split_while ~f:(Fn.compose not String.is_empty)
  in
  let query_lines = List.drop query_lines 1 in
  let db = parse_db db_lines in
  Int64.of_int
  @@ List.count query_lines ~f:(fun query ->
      let query = Int64.of_string query in
      List.exists db ~f:(fun (a, b) -> Int64.(a <= query && query <= b)))

let p2 input =
  let db =
    String.split_lines input
    |> List.take_while ~f:(Fn.compose not String.is_empty)
    |> parse_db
    |> List.concat_map ~f:(fun (l, r) -> [ Begin l; End Int64.(r + 1L) ])
    |> List.sort ~compare:(fun l r ->
        Int64.compare (extract_extent l) (extract_extent r))
  in
  let _, _, cnt =
    List.fold db ~init:(None, 0, 0L) ~f:(fun (start, nesting, cnt) -> function
      | Begin l -> (Some (Option.value start ~default:l), nesting + 1, cnt)
      | End r -> (
          match start with
          | None -> assert false
          | Some start ->
              if nesting = 1 then (None, 0, Int64.(cnt + r - start))
              else (Some start, nesting - 1, cnt)))
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
