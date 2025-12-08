open! Core

module Point = struct
  type t = int * int * int [@@deriving hash, compare, equal, sexp]

  let of_string s =
    match String.split s ~on:',' with
    | [ x; y; z ] -> (int_of_string x, int_of_string y, int_of_string z)
    | _ -> failwith "invalid point"

  let to_string (x, y, z) = sprintf "(%d,%d,%d)" x y z

  let dist_2 (x1, y1, z1) (x2, y2, z2) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))
end

module Graph = struct
  type t = (Point.t, (Point.t * int) list) Hashtbl.t

  let add_edge ~graph u v =
    Hashtbl.update graph u
      ~f:(Option.value_map ~default:[ v ] ~f:(fun acc -> v :: acc));
    Hashtbl.update graph v
      ~f:(Option.value_map ~default:[ u ] ~f:(fun acc -> u :: acc))

  let create_graph ~edges =
    let graph = Hashtbl.create (module Point) in
    List.iter edges ~f:(fun (a, b, _) -> add_edge ~graph a b);
    graph

  let dfs ~graph ~seen u =
    let result = ref [] in
    let rec go u =
      result := u :: !result;
      let neighbors = Hashtbl.find_exn graph u in
      List.iter neighbors ~f:(fun v ->
          if not @@ Hash_set.mem seen v then begin
            Hash_set.add seen v;
            go v
          end)
    in
    go u;
    !result

  let connected_components graph =
    let seen = Hash_set.create (module Point) in
    let vertices = Hashtbl.keys graph in
    List.fold vertices ~init:[] ~f:(fun acc u ->
        if not @@ Hash_set.mem seen u then begin
          Hash_set.add seen u;
          dfs ~graph ~seen u :: acc
        end
        else acc)
end

let rec get_pairs ?(seen = []) ?(acc = []) = function
  | [] -> acc
  | hd :: tl ->
      let acc = List.fold seen ~init:acc ~f:(fun acc y -> (y, hd) :: acc) in
      get_pairs ~seen:(hd :: seen) ~acc tl

let p1 ?(num = 10) input =
  let positions = String.split_lines input |> List.map ~f:Point.of_string in
  let edges =
    get_pairs positions
    |> List.map ~f:(fun (a, b) -> (a, b, Point.dist_2 a b))
    |> List.sort ~compare:(fun (_, _, x) (_, _, y) -> Int.compare x y)
  in
  let graph = Graph.create_graph ~edges:(List.take edges num) in
  let components = Graph.connected_components graph in
  let sizes =
    List.map components ~f:List.length
    |> List.sort ~compare:(Fn.flip Int.compare)
  in
  List.take sizes 3 |> List.fold ~init:1 ~f:( * )

let p2 input =
  let positions = String.split_lines input |> List.map ~f:Point.of_string in
  let num_vertices = List.length positions in
  let edges =
    get_pairs positions
    |> List.map ~f:(fun (a, b) -> (a, b, Point.dist_2 a b))
    |> List.sort ~compare:(fun (_, _, x) (_, _, y) -> Int.compare x y)
  in
  let graph = Hashtbl.create (module Point) in
  let (x1, _, _), (x2, _, _), _ =
    List.hd_exn
    @@ List.drop_while edges ~f:(fun (u, v, _) ->
        Graph.add_edge ~graph u v;
        List.length
          (Graph.dfs ~graph ~seen:(Hash_set.of_list (module Point) [ u ]) u)
        <> num_vertices)
  in
  x1 * x2

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d8p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 40 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d8p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 25272 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d8.in" in
  printf "%d" (p1 ~num:1000 input);
  [%expect {| 57564 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d8.in" in
  printf "%d" (p2 input);
  [%expect {| 133296744 |}]
