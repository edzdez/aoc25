open! Core

module Point = struct
  type t = int * int * int [@@deriving hash, compare, sexp]

  let of_string s =
    match String.split s ~on:',' with
    | [ x; y; z ] -> (int_of_string x, int_of_string y, int_of_string z)
    | _ -> failwith "invalid point"
end

let get_pairs = function
  | [] -> []
  | hd :: tl ->
      let _, pairs =
        List.fold tl ~init:([ hd ], []) ~f:(fun (seen, acc) x ->
            (x :: seen, acc @ List.map seen ~f:(fun y -> (y, x))))
      in
      pairs

let dist_2 (x1, y1, z1) (x2, y2, z2) =
  ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))

let p1 ?(num = 10) input =
  let positions = String.split_lines input |> List.map ~f:Point.of_string in
  let uf =
    List.map positions ~f:(fun p -> (p, Union_find.create p))
    |> Hashtbl.of_alist_exn (module Point)
  in
  let pairs =
    get_pairs positions
    |> List.sort ~compare:(fun (a, b) (x, y) ->
        Int.compare (dist_2 a b) (dist_2 x y))
  in
  List.take pairs num
  |> List.iter ~f:(fun (a, b) ->
      let uf_a = Hashtbl.find_exn uf a in
      let uf_b = Hashtbl.find_exn uf b in
      Union_find.union uf_a uf_b);
  let sizes = Hashtbl.create (module Point) in
  Hashtbl.iter uf ~f:(fun data ->
      Hashtbl.update sizes (Union_find.get data)
        ~f:(Option.value_map ~default:1 ~f:succ));
  let top_three =
    Hashtbl.data sizes |> List.sort ~compare:(Fn.flip Int.compare)
  in
  List.take top_three 3 |> List.fold ~init:1 ~f:( * )

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d8p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 40 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d8p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 40 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d8.in" in
  printf "%d" (p1 ~num:1000 input);
  [%expect {| 57564 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d8.in" in
  printf "%d" (p2 input);
  [%expect {| 17921968177009 |}]
*)
