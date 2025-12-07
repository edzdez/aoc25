open! Core

let parse_diagram input =
  let lines = String.split_lines input in
  let diagram =
    lines
    |> List.map ~f:(fun line ->
        String.to_list line
        |> List.map ~f:(fun c -> Char.(c = '^'))
        |> List.to_array)
  in
  let start =
    List.hd_exn lines
    |> String.findi ~f:(fun _ c -> Char.(c = 'S'))
    |> Option.value_exn |> fst
  in
  (diagram, start)

let simulate ~positions line =
  let cnt = ref 0 in
  let new_ =
    List.concat_map positions ~f:(fun pos ->
        if line.(pos) then begin
          cnt := !cnt + 1;
          if pos = 0 then [ 1 ]
          else if pos = Array.length line - 1 then [ pos - 1 ]
          else [ pos - 1; pos + 1 ]
        end
        else [ pos ])
    |> List.dedup_and_sort ~compare:Int.compare
  in
  (new_, !cnt)

let p1 input =
  let diagram, start = parse_diagram input in
  let _, cnt =
    List.fold diagram ~init:([ start ], 0) ~f:(fun (positions, cnt) line ->
        let positions, new_ = simulate ~positions line in
        (positions, cnt + new_))
  in
  cnt

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d7p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 21 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d7p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 3263827 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d7.in" in
  printf "%d" (p1 input);
  [%expect {| 1592 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d7.in" in
  printf "%d" (p2 input);
  [%expect {| 11744693538946 |}]
*)
