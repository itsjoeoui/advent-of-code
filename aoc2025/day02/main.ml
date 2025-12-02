type range = { s : int; e : int }

let parse_range (r : string) : range =
  match String.split_on_char '-' r with
  | [ st; en ] ->
      Printf.printf "Parsing range: %s - %s\n" st en;
      { s = int_of_string st; e = int_of_string en }
  | _ -> failwith "Invalid range format"

let parse_ranges (line : string) : range list =
  let ranges = String.split_on_char ',' line in
  List.map parse_range ranges

let is_repeating (num : int) (repeat : int) : bool =
  let s = string_of_int num in
  let len = String.length s in
  match len mod repeat <> 0 with
  | true -> false
  | _ ->
      let part_len = len / repeat in
      let part = String.sub s 0 part_len in

      let rec check_parts (i : int) : bool =
        if i >= repeat then true
        else
          let sub = String.sub s (i * part_len) part_len in
          if sub = part then check_parts (i + 1) else false
      in
      check_parts 1

let count_invalid (range : range) : int =
  let rec aux (num : int) (count : int) : int =
    if num > range.e then count
    else
      let new_count = if is_repeating num 2 then count + num else count in
      aux (num + 1) new_count
  in
  aux range.s 0

let count_invalid_part_2 (range : range) : int =
  let rec aux (num : int) (acc : int) : int =
    if num > range.e then acc
    else
      let s = string_of_int num in
      let len = String.length s in
      let rec is_invalid (repeat : int) : bool =
        if repeat <= 1 then false
        else if is_repeating num repeat then true
        else is_invalid (repeat - 1)
      in
      match is_invalid len with
      | true -> aux (num + 1) (acc + num)
      | false -> aux (num + 1) acc
  in
  aux range.s 0

let part1 (ranges : range list) : int =
  List.map count_invalid ranges |> List.fold_left ( + ) 0

let part2 (ranges : range list) : int =
  List.map count_invalid_part_2 ranges |> List.fold_left ( + ) 0

let read_input_all (filename : string) : string =
  In_channel.with_open_text filename In_channel.input_all |> String.trim

let solve (filename : string) : int * int =
  let all = read_input_all filename in
  let ranges = parse_ranges all in
  let result1 = part1 ranges in
  let result2 = part2 ranges in
  (result1, result2)

let () =
  let result1, result2 = solve "./day02/input.txt" in
  Printf.printf "Part 1: %d\n" result1;
  Printf.printf "Part 2: %d\n" result2
