open! Core

type point = int * int

let point_of_string s =
  match String.split_on_chars s ~on:[ ',' ] with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> failwith "invalid point"

let rec get_pairs ?(seen = []) ?(acc = []) = function
  | [] -> acc
  | hd :: tl ->
      let acc = List.fold seen ~init:acc ~f:(fun acc y -> (y, hd) :: acc) in
      get_pairs ~seen:(hd :: seen) ~acc tl

let rect_size (x1, y1) (x2, y2) = abs (x1 - x2 + 1) * abs (y1 - y2 + 1)

let rec get_edges = function
  | [] | [ _ ] -> []
  | x :: (y :: _ as tl) -> (x, y) :: get_edges tl

let min_max a b = if a < b then (a, b) else (b, a)

let cuts_rect (x1, y1) (x2, y2) ((xe1, ye1), (xe2, ye2)) =
  let xl, xr = min_max x1 x2 in
  let yb, yt = min_max y1 y2 in
  if xe1 = xe2 && xl < xe1 && xe1 < xr then
    let yeb, yet = min_max ye1 ye2 in
    not (yet <= yb || yeb >= yt)
  else if ye1 = ye2 && yb < ye1 && ye1 < yt then
    let xel, xer = min_max xe1 xe2 in
    not (xer <= xl || xel >= xr)
  else false

let p1 input =
  let points = String.split_lines input |> List.map ~f:point_of_string in
  let pairs = get_pairs points in
  List.fold pairs ~init:0 ~f:(fun best (a, b) -> Int.max best @@ rect_size a b)

let p2 input =
  let points = String.split_lines input |> List.map ~f:point_of_string in
  let edges = get_edges @@ points @ [ List.hd_exn points ] in
  let pairs = get_pairs points in
  List.fold pairs ~init:0 ~f:(fun best (a, b) ->
      if not @@ List.exists edges ~f:(cuts_rect a b) then
        Int.max best @@ rect_size a b
      else best)

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d9p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 50 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d9p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 24 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d9.in" in
  printf "%d" (p1 input);
  [%expect {| 4749672288 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d9.in" in
  printf "%d" (p2 input);
  [%expect {| 1479665889 |}]
