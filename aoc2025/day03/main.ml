let read_input_lines (filename : string) : string list =
  In_channel.with_open_text filename In_channel.input_lines

let largest (length : int) (line : string) : int =
  let line_length = String.length line in

  if line_length < length then 0
  else
    let find_max_digit_in_range start_pos end_pos =
      let rec aux pos max_pos =
        if pos > end_pos then max_pos
        else
          aux (pos + 1) (if line.[pos] > line.[max_pos] then pos else max_pos)
      in
      aux start_pos start_pos
    in

    let rec build_number pos remaining buffer =
      match remaining with
      | 0 -> Buffer.contents buffer |> int_of_string
      | _ ->
          let max_search_pos = line_length - remaining in
          let best_pos = find_max_digit_in_range pos max_search_pos in
          Buffer.add_char buffer line.[best_pos];
          build_number (best_pos + 1) (remaining - 1) buffer
    in

    build_number 0 length (Buffer.create length)

let part1 (lines : string list) : int =
  lines |> List.map (largest 2) |> List.fold_left ( + ) 0

let part2 (lines : string list) : int =
  lines |> List.map (largest 12) |> List.fold_left ( + ) 0

let () =
  let lines = read_input_lines "./day03/input.txt" in

  let part1_result = part1 lines in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 lines in
  Printf.printf "Part 2: %d\n" part2_result
