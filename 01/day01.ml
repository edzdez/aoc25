open! Core

type direction = Left | Right

let direction_of_string s =
  match s with "L" -> Left | "R" -> Right | _ -> failwith "Unknown direction!"

type rotation = { dir : direction; amt : int }
type state = { cnt : int; curr : int }

let rotation_of_string s : rotation =
  let hd = String.prefix s 1 in
  let tl = String.chop_prefix_exn s ~prefix:hd in
  { dir = direction_of_string hd; amt = int_of_string tl }

let int_of_rotation { dir; amt } = match dir with Left -> -amt | Right -> amt

let p1 input =
  let rotations = String.split_lines input |> List.map ~f:rotation_of_string in
  let { cnt; _ } =
    List.fold rotations ~init:{ cnt = 0; curr = 50 }
      ~f:(fun { cnt; curr } rot ->
        let new_ = (curr + int_of_rotation rot) mod 100 in
        { cnt = (if new_ = 0 then cnt + 1 else cnt); curr = new_ })
  in
  cnt

let p2 input =
  let rotations =
    String.split_lines input
    |> List.map ~f:(Fn.compose int_of_rotation rotation_of_string)
  in
  let { cnt; _ } =
    List.fold rotations ~init:{ cnt = 0; curr = 50 }
      ~f:(fun { cnt; curr } rot ->
        let new_ = curr + (rot mod 100) in
        let passed_zero =
          if new_ >= 100 || (curr > 0 && new_ <= 0) then 1 else 0
        in
        let rots = abs rot / 100 in
        { cnt = cnt + passed_zero + rots; curr = (new_ + 100) mod 100 })
  in
  cnt

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknow part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d1p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 3 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d1p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 6 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d1.in" in
  printf "%d" (p1 input);
  [%expect {| 1097 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d1.in" in
  printf "%d" (p2 input);
  [%expect {| 7101 |}]
