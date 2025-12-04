open! Core

let parse_grid input =
  String.split_lines input
  |> List.map ~f:(fun line ->
      String.to_list line
      |> List.map ~f:(fun c -> Char.(c = '@'))
      |> List.to_array)
  |> List.to_array

let in_grid ~rows ~cols (x, y) = x >= 0 && x < cols && y >= 0 && y < rows

let count_neighbors ~grid ~rows ~cols (x, y) =
  let neighbors = ref 0 in
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      let ((nx, ny) as p) = (x + dx, y + dy) in
      if (dx <> 0 || dy <> 0) && in_grid ~rows ~cols p && grid.(ny).(nx) then
        neighbors := !neighbors + 1
    done
  done;
  !neighbors

let find_accessible ~grid ~rows ~cols =
  let accessible = ref [] in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if grid.(row).(col) && count_neighbors ~grid ~rows ~cols (col, row) < 4
      then accessible := (col, row) :: !accessible
    done
  done;
  !accessible

let p1 input =
  let grid = parse_grid input in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  List.length @@ find_accessible ~grid ~rows ~cols

let p2 input =
  let grid = parse_grid input in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let rec aux ~num =
    let accessible = find_accessible ~grid ~rows ~cols in
    List.iter accessible ~f:(fun (col, row) -> grid.(row).(col) <- false);
    let new_ = List.length accessible in
    if new_ = 0 then num else aux ~num:(num + new_)
  in
  aux ~num:0

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d4p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 13 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d4p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 43 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d4.in" in
  printf "%d" (p1 input);
  [%expect {| 1356 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d4.in" in
  printf "%d" (p2 input);
  [%expect {| 8713 |}]
