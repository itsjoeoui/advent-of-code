type range = { s : int; e : int }

let parse_range (r : string) : range =
  match String.split_on_char '-' r with
  | [ st; en ] -> { s = int_of_string st; e = int_of_string en }
  | _ -> failwith "Invalid range format"

let parse_ranges (line : string) : range list =
  line |> String.split_on_char ',' |> List.map parse_range

let is_repeating (num : int) (repeat : int) : bool =
  let s = string_of_int num in
  let len = String.length s in
  len mod repeat = 0
  &&
  let part_len = len / repeat in
  let pattern = String.sub s 0 part_len in

  Seq.init repeat (fun i -> String.sub s (i * part_len) part_len)
  |> Seq.for_all (String.equal pattern)

let count_invalid ({ s; e } : range) : int =
  Seq.ints s
  |> Seq.take_while (fun n -> n <= e)
  |> Seq.filter (fun n -> is_repeating n 2)
  |> Seq.fold_left ( + ) 0

let has_repeating_pattern (num : int) : bool =
  let len = String.length (string_of_int num) in
  Seq.ints 2
  |> Seq.take_while (fun repeat -> repeat <= len)
  |> Seq.exists (is_repeating num)

let count_invalid_part_2 ({ s; e } : range) : int =
  Seq.ints s
  |> Seq.take_while (fun n -> n <= e)
  |> Seq.filter has_repeating_pattern
  |> Seq.fold_left ( + ) 0

let part1 (ranges : range list) : int =
  ranges |> List.map count_invalid |> List.fold_left ( + ) 0

let part2 (ranges : range list) : int =
  ranges |> List.map count_invalid_part_2 |> List.fold_left ( + ) 0

let solve (filename : string) : int * int =
  let ranges = Aoc2025.read_input_all filename |> parse_ranges in
  (part1 ranges, part2 ranges)

let () =
  let result1, result2 = solve "./day02/input.txt" in
  Printf.printf "Part 1: %d\n" result1;
  Printf.printf "Part 2: %d\n" result2
