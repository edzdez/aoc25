open! Core

module Node = struct
  type t = string * bool * bool [@@deriving equal, compare, hash, sexp]
end

module Graph : sig
  type 'a t

  val create : ?edges:('a * 'a) list -> 'a Hashtbl_intf.Hashtbl.Key.t -> 'a t

  val num_paths :
    graph:'a t ->
    equal:('a -> 'a -> bool) ->
    'a Hashtbl_intf.Hashtbl.Key.t ->
    'a ->
    ('a, int) Hashtbl.t
end = struct
  type 'a t = { adj : ('a, 'a list) Hashtbl.t; nodes : 'a Hash_set.t }

  let add_diredge ~graph:{ adj; nodes } u v =
    Hash_set.add nodes u;
    Hash_set.add nodes v;
    Hashtbl.update adj u
      ~f:(Option.value_map ~default:[ v ] ~f:(fun acc -> v :: acc))

  let create ?(edges = []) m =
    let graph = { adj = Hashtbl.create m; nodes = Hash_set.create m } in
    List.iter edges ~f:(fun (a, b) -> add_diredge ~graph a b);
    graph

  let toposort ~graph:{ adj; nodes } m =
    let in_deg = Hashtbl.create m in
    let rec go ?(acc = []) = function
      | [] -> List.rev acc
      | u :: tl -> begin
          match Hashtbl.find adj u with
          | None -> go ~acc:(u :: acc) tl
          | Some vs ->
              tl
              @ List.filter vs ~f:(fun v ->
                  let new_in_deg =
                    Hashtbl.update_and_return in_deg v ~f:(function
                      | None -> assert false
                      | Some c -> c - 1)
                  in
                  new_in_deg = 0)
              |> go ~acc:(u :: acc)
        end
    in
    Hashtbl.iter adj ~f:(fun vs ->
        List.iter vs
          ~f:(Hashtbl.update in_deg ~f:(function None -> 1 | Some e -> e + 1)));
    Hash_set.to_list nodes
    |> List.map ~f:(fun v ->
        Hashtbl.update in_deg v ~f:(function None -> 0 | Some e -> e);
        v)
    |> List.filter ~f:(fun v -> Hashtbl.find_exn in_deg v = 0)
    |> go

  let num_paths ~graph:({ adj; nodes } as graph) ~equal m v =
    let count = Hashtbl.create m in
    Hash_set.iter nodes ~f:(fun w ->
        Hashtbl.add_exn count ~key:w ~data:(if equal v w then 1 else 0));
    let order = List.rev @@ toposort ~graph m in
    List.iter order ~f:(fun w ->
        match Hashtbl.find adj w with
        | None -> ()
        | Some vs ->
            let num =
              List.fold vs ~init:0 ~f:(fun acc v ->
                  acc + Hashtbl.find_exn count v)
            in
            Hashtbl.set count ~key:w ~data:num);
    count
end

let edge_list_of_string s =
  let u, vs = String.lsplit2_exn s ~on:':' in
  let u = String.strip u in
  String.strip vs
  |> String.split_on_chars ~on:[ ' ' ]
  |> List.map ~f:(fun v -> (u, v))

let p1 input =
  let edges =
    String.split_lines input |> List.concat_map ~f:edge_list_of_string
  in
  let graph = Graph.create ~edges (module String) in
  Graph.num_paths ~graph (module String) ~equal:String.equal "out"
  |> Fn.flip Hashtbl.find_exn "you"

let p2 input =
  let edges =
    String.split_lines input
    |> List.concat_map ~f:(fun line ->
        let edges = edge_list_of_string line in
        List.concat_map edges ~f:(fun (u, v) ->
            match u with
            | "dac" ->
                [
                  ((u, false, false), (v, true, false));
                  ((u, false, true), (v, true, true));
                ]
            | "fft" ->
                [
                  ((u, false, false), (v, false, true));
                  ((u, true, false), (v, true, true));
                ]
            | u ->
                [
                  ((u, false, false), (v, false, false));
                  ((u, true, false), (v, true, false));
                  ((u, false, true), (v, false, true));
                  ((u, true, true), (v, true, true));
                ]))
  in
  let graph = Graph.create ~edges (module Node) in
  Graph.num_paths ~graph (module Node) ~equal:Node.equal ("out", true, true)
  |> Fn.flip Hashtbl.find_exn @@ ("svr", false, false)

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d11p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 5 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d11p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 2 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p1 input);
  [%expect {| 668 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p2 input);
  [%expect {| 294310962265680 |}]
