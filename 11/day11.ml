open! Core

module Graph = struct
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

  let _dfs ~graph ~seen u =
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

  let toposort ~graph:{ adj; nodes } =
    let in_deg : ('a, int) Hashtbl.t = Hashtbl.create (module String) in
    let rec go ?(acc = []) = function
      | [] -> List.rev acc
      | u :: tl -> (
          match Hashtbl.find adj u with
          | None -> go ~acc:(u :: acc) tl
          | Some vs ->
              go ~acc:(u :: acc)
              @@ tl
              @ List.filter vs ~f:(fun v ->
                  let new_in_deg =
                    Hashtbl.update_and_return in_deg v ~f:(function
                      | None -> assert false
                      | Some c -> c - 1)
                  in
                  new_in_deg = 0))
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

  let num_paths ~graph:({ adj; nodes } as graph) v =
    let count = Hashtbl.create (module String) in
    Hash_set.iter nodes ~f:(fun w ->
        Hashtbl.add_exn count ~key:w ~data:(if String.equal v w then 1 else 0));
    let order = List.rev @@ toposort ~graph in
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
  let num_paths = Graph.num_paths ~graph "out" in
  Hashtbl.find_exn num_paths "you"

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d11p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 5 |}]

(*
let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d11p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 24 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p1 input);
  [%expect {| 668 |}]

(*
let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d11.in" in
  printf "%d" (p2 input);
  [%expect {| 1479665889 |}]
*)
