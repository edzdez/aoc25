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

let do_math nums ops =
  List.zip_exn nums ops
  |> List.fold ~init:0 ~f:(fun acc (nums, op) ->
      acc
      +
      match op with
      | Plus -> List.fold nums ~init:0 ~f:( + )
      | Times -> List.fold nums ~init:1 ~f:( * ))

let p1 input =
  let lines = String.split_lines input in
  let nums, ops = List.split_n lines (List.length lines - 1) in
  let nums, ops =
    (List.transpose_exn @@ parse_nums nums, parse_ops @@ List.hd_exn ops)
  in
  do_math nums ops

let p2 input =
  (*
  let parse_ops input =
    let rec aux ~acc ~curr = function
      | [] -> List.drop (List.rev (curr :: acc)) 1
      | hd :: tl -> (
          match (hd, curr) with
          | '+', (op, cnt) -> aux ~acc:((op, cnt - 1) :: acc) ~curr:(Plus, 1) tl
          | '*', (op, cnt) ->
              aux ~acc:((op, cnt - 1) :: acc) ~curr:(Times, 1) tl
          | ' ', (op, cnt) -> aux ~acc ~curr:(op, cnt + 1) tl
          | _ -> assert false)
    in
    aux ~acc:[] ~curr:(Plus, 0) @@ String.to_list input
  in
  let parse_nums ~acc
  let do_math op nums =
    match op with
    | Plus -> List.fold nums ~init:0 ~f:( + )
    | Times -> List.fold nums ~init:1 ~f:( * )
  in
  *)
  let rec parse_nums ~acc ~curr = function
    | [] -> List.rev @@ (curr :: acc)
    | hd :: tl -> (
        match int_of_string_opt (String.strip hd) with
        | Some n -> parse_nums ~acc ~curr:(n :: curr) tl
        | None -> parse_nums ~acc:(List.rev curr :: acc) ~curr:[] tl)
  in
  let lines = String.split_lines input in
  let nums, ops = List.split_n lines (List.length lines - 1) in
  let ops = parse_ops @@ List.hd_exn ops in
  let len =
    Option.value ~default:0
    @@ List.max_elt ~compare:Int.compare
    @@ List.map nums ~f:String.length
  in
  let nums =
    nums
    |> List.map ~f:(Fn.compose String.to_list (String.pad_right ~char:' ' ~len))
    |> List.transpose_exn
    |> List.map ~f:String.of_char_list
    |> parse_nums ~acc:[] ~curr:[]
  in
  do_math nums ops

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d6p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 4277556 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d6p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 3263827 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p1 input);
  [%expect {| 5361735137219 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d6.in" in
  printf "%d" (p2 input);
  [%expect {| 11744693538946 |}]
