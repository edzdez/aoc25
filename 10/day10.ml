open! Core

module Graph : sig
  type 'a t

  val add_diredge : graph:'a t -> 'a -> 'a -> unit
  val create : ?edges:('a * 'a) list -> 'a Hashtbl_intf.Hashtbl.Key.t -> 'a t

  val distance :
    graph:'a t ->
    'a Hashtbl_intf.Hashtbl.Key.t ->
    'a ->
    'a ->
    equal:('a -> 'a -> bool) ->
    int option
end = struct
  type 'a t = ('a, 'a list) Hashtbl.t

  let add_diredge ~graph u v =
    Hashtbl.update graph u
      ~f:(Option.value_map ~default:[ v ] ~f:(fun acc -> v :: acc))

  let add_edge ~graph u v =
    add_diredge ~graph u v;
    add_diredge ~graph v u

  let create ?(edges = []) m =
    let graph = Hashtbl.create m in
    List.iter edges ~f:(fun (a, b) -> add_edge ~graph a b);
    graph

  let distance ~graph m u v ~equal =
    let q = Queue.of_list [ (u, 0) ] in
    let visited = Hash_set.of_list m [ u ] in
    let rec go () =
      match Queue.dequeue q with
      | None -> None
      | Some (u, d) -> begin
          if equal u v then Some d
          else
            match Hashtbl.find graph u with
            | None -> None
            | Some neighbors ->
                List.iter neighbors ~f:(fun v ->
                    if not @@ Hash_set.mem visited v then begin
                      Hash_set.add visited v;
                      Queue.enqueue q (v, d + 1)
                    end);
                go ()
        end
    in
    go ()
end

let button_of_string s =
  String.strip s ~drop:(fun c -> Char.(c = '(' || c = ')'))
  |> String.split_on_chars ~on:[ ',' ]
  |> List.fold ~init:0 ~f:(fun acc c ->
      let i = int_of_string c in
      Int.bit_or acc (Int.shift_left 1 i))

let lights_of_string s =
  String.strip s ~drop:(fun c -> Char.(c = '[' || c = ']'))
  |> String.to_list
  |> List.foldi ~init:0 ~f:(fun i acc c ->
      if Char.(c = '#') then Int.bit_or acc (Int.shift_left 1 i) else acc)

let joltages_of_string s =
  String.strip s ~drop:(fun c -> Char.(c = '{' || c = '}'))
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:int_of_string

type machine = {
  size : int;
  config : int;
  buttons : int list;
  joltages : int list;
}

let machine_of_string s =
  String.split_on_chars s ~on:[ ' ' ]
  |> List.fold ~init:{ size = 0; config = 0; buttons = []; joltages = [] }
       ~f:(fun ({ buttons; _ } as machine) chunk ->
         match String.prefix chunk 1 with
         | "[" ->
             let config = lights_of_string chunk in
             { machine with size = String.length chunk - 2; config }
         | "(" ->
             let button = button_of_string chunk in
             { machine with buttons = button :: buttons }
         | "{" ->
             let joltages = joltages_of_string chunk in
             { machine with joltages }
         | _ -> failwith "unknown chunk prefix.")

let min_presses { size; config; buttons; _ } =
  let graph = Graph.create (module Int) in
  for state = 0 to Int.shift_left 2 size - 1 do
    List.iter buttons ~f:(fun toggle ->
        let new_state = Int.bit_xor state toggle in
        Graph.add_diredge ~graph state new_state)
  done;
  Option.value_exn @@ Graph.distance ~graph (module Int) 0 config ~equal:( = )

let min_presses_ilp { buttons; joltages; _ } =
  let open Z3 in
  let ctx = mk_context [] in
  let opt = Optimize.mk_opt ctx in
  let vars =
    Array.of_list_mapi buttons ~f:(fun i _ ->
        Symbol.mk_int ctx i |> Arithmetic.Integer.mk_const ctx)
  in
  let constraints =
    List.mapi joltages ~f:(fun i target_joltage ->
        let lhs =
          List.filter_mapi buttons ~f:(fun but_i button ->
              if Int.bit_and button (Int.shift_left 1 i) <> 0 then
                Some vars.(but_i)
              else None)
          |> Arithmetic.mk_add ctx
        in
        let rhs = Arithmetic.Integer.mk_numeral_i ctx target_joltage in
        Boolean.mk_eq ctx lhs rhs)
  in
  let non_negativity =
    Array.map vars ~f:(fun v ->
        Arithmetic.mk_ge ctx v (Arithmetic.Integer.mk_numeral_i ctx 0))
    |> List.of_array
  in
  let obj = List.of_array vars |> Arithmetic.mk_add ctx in
  Optimize.add opt constraints;
  Optimize.add opt non_negativity;
  let handle = Optimize.minimize opt obj in
  match Optimize.check opt with
  | SATISFIABLE -> Optimize.get_upper handle |> Expr.to_string |> int_of_string
  | _ -> failwith "infeasible"

let p1 input =
  let machines = String.split_lines input |> List.map ~f:machine_of_string in
  List.fold machines ~init:0 ~f:(fun acc m -> acc + min_presses m)

let p2 input =
  let machines = String.split_lines input |> List.map ~f:machine_of_string in
  List.fold machines ~init:0 ~f:(fun acc m -> acc + min_presses_ilp m)

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d10p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 7 |}]

let%expect_test "part 2 sample input" =
  let input = In_channel.read_all "../inputs/d10p2.sample" in
  printf "%d" (p2 input);
  [%expect {| 33 |}]

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d10.in" in
  printf "%d" (p1 input);
  [%expect {| 415 |}]

let%expect_test "part 2" =
  let input = In_channel.read_all "../inputs/d10.in" in
  printf "%d" (p2 input);
  [%expect {| 16663 |}]
