type range = { s : int; e : int }

let parse_range (line : string) : range =
  match String.split_on_char '-' line with
  | [ fst; snd ] -> { s = int_of_string fst; e = int_of_string snd }
  | _ -> failwith "parse_range: expected format 'start-end'"

let parse_input (lines : string list) : range list * int list =
  let ranges, nums, _ =
    List.fold_left
      (fun ( (ranges : range list),
             (ingredient_ids : int list),
             (is_parsing_ranges : bool) ) (line : string) :
           (range list * int list * bool) ->
        if line = "" then (ranges, ingredient_ids, false)
        else
          match is_parsing_ranges with
          | true -> (parse_range line :: ranges, ingredient_ids, true)
          | false -> (ranges, int_of_string line :: ingredient_ids, false))
      ([], [], true) lines
  in
  (ranges, nums)

let compact_ranges (ranges : range list) : range list =
  let sorted =
    List.sort
      (fun r1 r2 ->
        match compare r1.s r2.s with 0 -> compare r1.e r2.e | c -> c)
      ranges
  in
  let rec merge acc = function
    | [] -> List.rev acc
    | curr :: rest -> (
        match acc with
        | [] -> merge [ curr ] rest
        | last :: rest_acc ->
            if curr.s <= last.e then
              merge ({ s = last.s; e = max last.e curr.e } :: rest_acc) rest
            else merge (curr :: acc) rest)
  in
  merge [] sorted

let part1 (ranges : range list) (ingredient_ids : int list) : int =
  ingredient_ids
  |> List.fold_left
       (fun acc ingredient_id ->
         match
           List.exists
             (fun range -> ingredient_id <= range.e && ingredient_id >= range.s)
             ranges
         with
         | true -> acc + 1
         | false -> acc)
       0

let part2 (ranges : range list) : int =
  ranges |> List.fold_left (fun acc range -> acc + range.e - range.s + 1) 0

let () =
  let input = Aoc2025.read_input_lines "./day05/input.txt" in

  let range_list, ingredient_id_list = parse_input input in

  let compacted_ranges = compact_ranges range_list in

  let part1_result = part1 compacted_ranges ingredient_id_list in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 compacted_ranges in
  Printf.printf "Part 2: %d\n" part2_result
