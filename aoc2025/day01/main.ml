type direction = Left | Right
type rotation = { dir : direction; distance : int }

let read_input (filename : string) : string list =
  In_channel.with_open_text filename In_channel.input_lines

let parse_rotation (line : string) =
  match line.[0] with
  | 'L' ->
      {
        dir = Left;
        distance = int_of_string (String.sub line 1 (String.length line - 1));
      }
  | 'R' ->
      {
        dir = Right;
        distance = int_of_string (String.sub line 1 (String.length line - 1));
      }
  | _ -> failwith "Invalid direction"

let apply_rotation (pos : int) ({ dir; distance } : rotation) : int =
  let new_pos =
    match dir with Left -> pos - distance | Right -> pos + distance
  in
  ((new_pos mod 100) + 100) mod 100

let part1 (rotations : rotation list) : int =
  let rec process (pos : int) (rots : rotation list) (count : int) : int =
    match rots with
    | [] -> count
    | rot :: rest ->
        let new_pos = apply_rotation pos rot in
        let new_count = match new_pos with 0 -> count + 1 | _ -> count in
        process new_pos rest new_count
  in
  process 50 rotations 0

let part2 (rotations : rotation list) : int =
  let rec process (pos : int) (rots : rotation list) (count : int) : int =
    match rots with
    | [] -> count
    | ({ dir; distance } as rot) :: rest ->
        let new_pos = apply_rotation pos rot in
        let new_count =
          count
          +
          match dir with
          | Left -> (
              match pos with
              | 0 -> distance / 100
              | _ -> if pos > distance then 0 else 1 + ((distance - pos) / 100))
          | Right ->
              if 100 - pos > distance then 0
              else 1 + ((distance - (100 - pos)) / 100)
        in
        process new_pos rest new_count
  in
  process 50 rotations 0

let solve (filename : string) : int * int =
  let lines = read_input filename in
  let rotations = List.map parse_rotation lines in
  let result1 = part1 rotations in
  let result2 = part2 rotations in
  (result1, result2)

let () =
  let result1, result2 = solve "./day01/input.txt" in
  Printf.printf "Part 1: %d\n" result1;
  Printf.printf "Part 2: %d\n" result2
