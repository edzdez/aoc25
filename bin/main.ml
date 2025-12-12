open! Core
open! Core_unix

let run ~day ~part file () =
  let input =
    match file with
    | "-" -> In_channel.input_all In_channel.stdin
    | _ -> In_channel.read_all file
  in
  let result =
    match day with
    | 1 -> string_of_int @@ Day01.run ~part input
    | 2 -> Int64.to_string @@ Day02.run ~part input
    | 3 -> Int64.to_string @@ Day03.run ~part input
    | 4 -> string_of_int @@ Day04.run ~part input
    | 5 -> Int64.to_string @@ Day05.run ~part input
    | 6 -> string_of_int @@ Day06.run ~part input
    | 7 -> string_of_int @@ Day07.run ~part input
    | 8 -> string_of_int @@ Day08.run ~part input
    | 9 -> string_of_int @@ Day09.run ~part input
    | 10 -> string_of_int @@ Day10.run ~part input
    | 11 -> string_of_int @@ Day11.run ~part input
    | 12 -> string_of_int @@ Day12.run ~part input
    | _ -> failwith "Unimplemented!"
  in
  printf "Day %d, part %d: %s\n" day part result

let () =
  Command.basic ~summary:"Advent of code driver"
    (let%map_open.Command day = anon ("day" %: int)
     and part =
       flag "-part" (optional_with_default 1 int) ~doc:" the part to run"
     and file =
       flag "-file"
         (optional_with_default "-" string)
         ~doc:" path to the input file"
     in
     run ~day ~part file)
  |> Command_unix.run
