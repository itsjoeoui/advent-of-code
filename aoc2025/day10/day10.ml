open Core

type group = int list

type problem = {
  states : bool array;
  groups : group array;
  joltages : int array;
}

let parse_problem (line : string) : problem =
  let rec parse states groups joltages items =
    match items with
    | [] ->
        {
          states = Array.of_list states;
          groups = Array.of_list groups;
          joltages = Array.of_list joltages;
        }
    | item :: rest -> (
        match item.[0] with
        | '[' ->
            let states =
              String.sub item ~pos:1 ~len:(String.length item - 2)
              |> String.to_list
              |> List.map ~f:(function
                | '.' -> false
                | '#' -> true
                | _ -> failwith "Invalid state character")
            in
            parse states groups joltages rest
        | '(' ->
            let group =
              String.sub item ~pos:1 ~len:(String.length item - 2)
              |> String.split ~on:',' |> List.map ~f:int_of_string
            in
            parse states (group :: groups) joltages rest
        | '{' ->
            let joltages =
              String.sub item ~pos:1 ~len:(String.length item - 2)
              |> String.split ~on:',' |> List.map ~f:int_of_string
            in
            parse states groups joltages rest
        | _ -> failwith ("Unknown item prefix: " ^ item))
  in
  let items = String.split line ~on:' ' in
  parse [] [] [] items

let parse_problems (lines : string list) : problem array =
  lines |> List.map ~f:parse_problem |> Array.of_list

(* TODO: part1 *)
let part1 (problems : problem array) : int = Array.length problems

(* TODO: part2 *)
let part2 (problems : problem array) : int = Array.length problems

let () =
  let lines = Aoc2025.read_input_lines "./day10/input.txt" in

  let problems = parse_problems lines in

  let part1_result = part1 problems in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 problems in
  Printf.printf "Part 2: %d\n" part2_result
