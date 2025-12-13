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

let p1 input =
  let shapes, regions = parse_summary input in
  List.count regions ~f:(fun { width; height; demand } ->
      let necessary_size =
        List.zip_exn shapes demand
        |> List.fold ~init:0 ~f:(fun acc (shape, need) ->
            acc + (need * List.length shape))
      in
      necessary_size <= width * height)

let p2 _input = assert false

let run ~part input =
  match part with
  | 1 -> p1 input
  | 2 -> p2 input
  | _ -> failwith (sprintf "Unknown part: %d" part)

let%expect_test "part 1" =
  let input = In_channel.read_all "../inputs/d12.in" in
  printf "%d" (p1 input);
  [%expect {| 557 |}]
