open Core

type coord = { x : int; y : int }

let index_combs_of_2 n =
  let rec loop i j acc =
    if i >= n then acc
    else if j >= n then loop (i + 1) (i + 2) acc
    else loop i (j + 1) ((i, j) :: acc)
  in
  loop 0 1 []

let parse_problem (lines : string list) : coord array =
  lines
  |> List.map ~f:(fun line -> Scanf.sscanf line "%d,%d" (fun x y -> { x; y }))
  |> Array.of_list

let area c1 c2 =
  let dx = Int.abs (c1.x - c2.x) + 1 in
  let dy = Int.abs (c1.y - c2.y) + 1 in
  dx * dy

let part1 (coords : coord array) : int =
  let n = Array.length coords in
  index_combs_of_2 n
  |> List.map ~f:(fun (i, j) -> area coords.(i) coords.(j))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

(* TODO: part2 *)
let part2 (coords : coord array) : int = Array.length coords

let () =
  let lines = Aoc2025.read_input_lines "./day09/input.txt" in

  let coords = parse_problem lines in

  let part1_result = part1 coords in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 coords in
  Printf.printf "Part 2: %d\n" part2_result
