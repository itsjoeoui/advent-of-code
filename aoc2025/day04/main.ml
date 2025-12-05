type grid = char array array

let directions =
  [ (0, 1); (1, 0); (0, -1); (-1, 0); (1, 1); (1, -1); (-1, 1); (-1, -1) ]

let remove (grid : grid) : grid * int =
  let rows = Array.length grid in
  let cols = if rows = 0 then 0 else Array.length grid.(0) in

  let in_bound (row : int) (col : int) : bool =
    row >= 0 && row < rows && col >= 0 && col < cols
  in

  let should_remove (grid : grid) (row : int) (col : int) : bool =
    match grid.(row).(col) with
    | '@' ->
        directions
        |> List.filter (fun (dr, dc) ->
            let r, c = (row + dr, col + dc) in
            in_bound r c && grid.(r).(c) == '@')
        |> List.length
        |> fun len -> len < 4
    | _ -> false
  in

  let positions_to_update =
    List.init rows Fun.id
    |> List.concat_map (fun row ->
        List.init cols (fun col -> (row, col))
        |> List.filter (fun (row, col) -> should_remove grid row col))
  in

  List.iter (fun (row, col) -> grid.(row).(col) <- '.') positions_to_update;
  (grid, List.length positions_to_update)

let part1 (grid : grid) : int =
  let _, count = remove grid in
  count

let part2 (grid : grid) : int =
  let rec solve (grid : grid) (count : int) : int =
    let new_grid, new_count = remove grid in
    match new_count with 0 -> count | _ -> solve new_grid (count + new_count)
  in
  solve grid 0

let parse_grid (input : string list) : grid =
  input
  |> List.map (fun line -> line |> String.to_seq |> Array.of_seq)
  |> Array.of_list

let () =
  let input = Aoc2025.read_input_lines "./day04/input.txt" in

  let grid1 = parse_grid input in
  let grid2 = parse_grid input in

  let part1_result = part1 grid1 in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 grid2 in
  Printf.printf "Part 2: %d\n" part2_result
