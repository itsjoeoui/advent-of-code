type dim = { width : int; height : int }
type problem = { dim : dim; count : int }

let part1 (problems : problem list) : int =
  problems
  |> List.fold_left
       (fun acc p ->
         if p.dim.width / 3 * (p.dim.height / 3) >= p.count then acc + 1
         else acc)
       0

let parse_dim (dim : string) : dim =
  match dim |> String.split_on_char 'x' |> List.map int_of_string with
  | [ w; h ] -> { width = w; height = h }
  | _ -> failwith "invalid dim"

let parse_nums (nums : string) : int =
  nums |> String.trim |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string |> List.fold_left ( + ) 0

let parse_lines (lines : string list) : problem list =
  let rec parse acc lines =
    match lines with
    | [] -> acc
    | x :: xs -> (
        match String.contains x 'x' with
        | false -> parse acc xs
        | true -> (
            let parts = String.split_on_char ':' x in
            match parts with
            | [ dims; nums ] ->
                parse
                  ({ count = parse_nums nums; dim = parse_dim dims } :: acc)
                  xs
            | _ -> failwith "invalid problem"))
  in
  parse [] lines

let () =
  let lines = Aoc2025.read_input_lines "./day12/input.txt" in

  let problems = parse_lines lines in

  let part1_result = part1 problems in
  Printf.printf "Part 1: %d\n" part1_result
