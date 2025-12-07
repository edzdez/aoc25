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
  (diagram, Map.of_alist_exn (module Int) [ (start, 1) ])

let simulate ~positions ?(dedup = true) line =
  let len = Array.length line in
  let cnt = ref 0 in
  let new_ =
    Map.fold positions
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data acc ->
        (if line.(key) then begin
           cnt := !cnt + if dedup then 1 else data;
           [ key - 1; key + 1 ]
         end
         else [ key ])
        |> List.filter ~f:(fun pos -> pos >= 0 && pos < len)
        |> List.fold ~init:acc ~f:(fun acc pos ->
            Map.update acc pos
              ~f:(Option.value_map ~default:data ~f:(fun v -> v + data))))
  in
  (new_, !cnt)

let p1 input =
  let diagram, start = parse_diagram input in
  let _, cnt =
    List.fold diagram ~init:(start, 0) ~f:(fun (positions, cnt) line ->
        let positions, new_ = simulate ~positions line in
        (positions, cnt + new_))
  in
  cnt

let p2 input =
  let diagram, start = parse_diagram input in
  let _, cnt =
    List.fold diagram ~init:(start, 0) ~f:(fun (positions, cnt) line ->
        let positions, new_ = simulate ~positions ~dedup:false line in
        (positions, cnt + new_))
  in
  cnt + 1

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d7p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 21 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d7p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 40 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d7.in" in
  printf "%d" (p1 input);
  [%expect {| 1592 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d7.in" in
  printf "%d" (p2 input);
  [%expect {| 17921968177009 |}]
