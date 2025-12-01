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

let p1 input =
  let rotations = String.split_lines input |> List.map ~f:rotation_of_string in
  let { cnt; _ } =
    List.fold rotations ~init:{ cnt = 0; curr = 50 }
      ~f:(fun { cnt; curr } { dir; amt } ->
        let new_ =
          (match dir with Left -> curr - amt | Right -> curr + amt) mod 100
        in
        { cnt = (if new_ = 0 then cnt + 1 else cnt); curr = new_ })
  in
  cnt

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknow part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d1p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 3 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d1.in" in
  printf "%d" (p1 input);
  [%expect {| 1097 |}]
