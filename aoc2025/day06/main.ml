type op = Add | Multiply
type problem = { op : op; nums : int list }

let parse_op = function
  | "+" -> Add
  | "*" -> Multiply
  | s -> failwith ("Unknown operation: " ^ s)

let part1 (problems : problem list) : int =
  problems
  |> List.map (fun p ->
      match p.op with
      | Add -> List.fold_left ( + ) 0 p.nums
      | Multiply -> List.fold_left ( * ) 1 p.nums)
  |> List.fold_left ( + ) 0

let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | (x :: xs) :: xss ->
      let heads = x :: List.map List.hd xss in
      let tails = xs :: List.map List.tl xss in
      heads :: transpose tails

let part2 (lines : string list) : int =
  let input =
    lines |> List.map String.to_seq |> List.map List.of_seq |> List.map List.rev
    |> transpose |> List.concat
  in
  let rec solve acc temp nums input =
    match input with
    | [] -> acc
    | x :: xs -> (
        match x with
        | ' ' ->
            if temp == "" then solve acc "" nums xs
            else solve acc "" (int_of_string temp :: nums) xs
        | '+' ->
            if temp == "" then
              solve (acc + List.fold_left ( + ) 0 nums) "" [] xs
            else
              solve
                (acc + List.fold_left ( + ) 0 (int_of_string temp :: nums))
                "" [] xs
        | '*' ->
            if temp == "" then
              solve (acc + List.fold_left ( * ) 1 nums) "" [] xs
            else
              solve
                (acc + List.fold_left ( * ) 1 (int_of_string temp :: nums))
                "" [] xs
        | _ -> solve acc (temp ^ String.make 1 x) nums xs)
  in
  solve 0 "" [] input

let parse_lines (lines : string list) : problem list =
  lines
  |> List.map (fun line ->
      line |> String.split_on_char ' ' |> List.filter (fun s -> s <> ""))
  |> transpose
  |> List.map (fun raw ->
      let rev = List.rev raw in
      match rev with
      | x :: xs -> { op = parse_op x; nums = xs |> List.map int_of_string }
      | _ -> failwith "invalid line")

let () =
  let lines = Aoc2025.read_input_lines "./day06/input.txt" in

  let problems = parse_lines lines in

  let part1_result = part1 problems in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 lines in
  Printf.printf "Part 2: %d\n" part2_result
