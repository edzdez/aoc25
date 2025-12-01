open! Core

let p1 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> assert false
  | _ -> failwith (sprintf "Unknow part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d1p1.sample" in
  printf "%d" (p1 input);
  [%expect]
