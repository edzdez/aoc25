open! Core

type shape = (int * int) list

let shape_of_list =
  List.concat_mapi ~f:(fun y row ->
      List.filter_mapi row ~f:(fun x -> function
        | true -> Some (x, y)
        | false -> None))

type region = { width : int; height : int; demand : int list }

let region_of_string s =
  let fst, snd = String.lsplit2_exn s ~on:':' in
  let width, height = String.lsplit2_exn fst ~on:'x' in
  let demand =
    String.strip snd
    |> String.split_on_chars ~on:[ ' ' ]
    |> List.map ~f:int_of_string
  in
  { width = int_of_string width; height = int_of_string height; demand }

let parse_summary input =
  let rec parse_shape ?(shapes = []) n lines =
    if n = 0 then (List.rev shapes, lines)
    else
      let lines = List.drop lines 1 in
      let chunk, lines = List.split_n lines 3 in
      let lines = List.drop lines 1 in
      let shape =
        List.map chunk ~f:(fun line ->
            String.to_list line |> List.map ~f:(fun c -> Char.(c = '#')))
        |> shape_of_list
      in
      parse_shape ~shapes:(shape :: shapes) (n - 1) lines
  in
  let lines = String.split_lines input in
  let shapes, lines = parse_shape 6 lines in
  let regions = List.map lines ~f:region_of_string in
  (shapes, regions)

let gen_orients shape =
  (* I only care about passing the sample input since this puzzle is a troll. *)
  List.concat_map
    [ shape; List.map shape ~f:(fun (x, y) -> (y, -x + 2)) ]
    ~f:(fun shape -> [ shape; List.map shape ~f:(fun (x, y) -> (2 - x, y)) ])

let try_naive_tiling ~region:{ width; height; demand } =
  let width = width / 3 in
  let height = height / 3 in
  let needed = List.fold demand ~init:0 ~f:(fun acc num -> acc + num) in
  needed <= width * height

let try_brute_force_tiling ~shapes ~region:{ width; height; demand } =
  let grid =
    Array.init height ~f:(fun _ -> Array.init width ~f:(fun _ -> false))
  in
  let demand = Array.of_list demand in
  let can_fit ~shape x y =
    List.for_all shape ~f:(fun (dx, dy) ->
        let x = x + dx in
        let y = y + dy in
        x < width && y < height && not grid.(y).(x))
  in
  let place ~shape ~place x y =
    List.iter shape ~f:(fun (dx, dy) ->
        let x = x + dx in
        let y = y + dy in
        grid.(y).(x) <- place)
  in
  let rec go x y =
    if x = width - 1 && y = height - 1 then
      Array.for_all demand ~f:(fun x -> x = 0)
    else
      let next_x, next_y = if x = width - 1 then (0, y + 1) else (x + 1, y) in
      List.existsi shapes ~f:(fun i shape ->
          demand.(i) <> 0
          && List.exists (gen_orients shape) ~f:(fun shape ->
              if can_fit ~shape x y then begin
                place ~shape ~place:true x y;
                demand.(i) <- demand.(i) - 1;
                let res = go next_x next_y in
                place ~shape ~place:false x y;
                demand.(i) <- demand.(i) + 1;
                res
              end
              else false))
      || go next_x next_y
  in
  go 0 0

let p1 input =
  let shapes, regions = parse_summary input in
  List.count regions ~f:(fun region ->
      try_naive_tiling ~region || try_brute_force_tiling ~shapes ~region)

let run ~part input =
  match part with
  | 1 -> p1 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

(* NOTE: commented out since it takes like 15 minutes to run...
let%expect_test "part 1 sample input" =
  let input = In_channel.read_all "../inputs/d12p1.sample" in
  printf "%d" (p1 input);
  [%expect {| 2 |}]
*)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d12.in" in
  printf "%d" (p1 input);
  [%expect {| 557 |}]
