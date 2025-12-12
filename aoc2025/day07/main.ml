module IntSet = Set.Make (Int)

type level = int list
type problem = { levels : level list; start : int }
type comparison = Less | Equal | Greater

let compare_numbers x y =
  match Stdlib.compare x y with
  | 0 -> Equal
  | z when z < 0 -> Less
  | _ -> Greater

let solve_level level beams =
  let rec solve level beams new_beams count =
    match (level, beams) with
    | [], [] -> (IntSet.elements new_beams, count)
    | [], beams ->
        let union = IntSet.union new_beams (IntSet.of_list beams) in
        (IntSet.elements union, count)
    | _, [] -> (IntSet.elements new_beams, count)
    | xl :: xsl, xb :: xsb -> (
        match compare_numbers xl xb with
        | Equal ->
            let new_beams =
              new_beams |> IntSet.add (xl - 1) |> IntSet.add (xl + 1)
            in
            solve xsl xsb new_beams (count + 1)
        | Less -> solve xsl beams new_beams count
        | Greater -> solve level xsb (IntSet.add xb new_beams) count)
  in
  solve level beams IntSet.empty 0

let part1 problem : int =
  let rec solve (levels : level list) (beams : int list) (acc : int) =
    match levels with
    | [] -> acc
    | x :: xs ->
        let new_beams, count = solve_level x beams in
        solve xs new_beams acc + count
  in
  solve problem.levels [ problem.start ] 0

let part2 problem : int = problem.start

let parse_problem (lines : string list) : problem =
  let rec parse start levels lines =
    match start with
    | None ->
        let start = String.index_from (List.hd lines) 0 'S' in
        parse (Some start) levels (List.tl lines)
    | Some start -> (
        match lines with
        | [] -> { levels = List.rev levels; start }
        | x :: xs ->
            let splits =
              x |> String.to_seq
              |> Seq.mapi (fun i c -> (i, c))
              |> Seq.filter_map (fun (i, c) -> if c = '^' then Some i else None)
              |> List.of_seq
            in
            parse (Some start) (splits :: levels) xs)
  in
  parse None [] lines

let () =
  let lines = Aoc2025.read_input_lines "./day07/input.txt" in

  let problem = parse_problem lines in

  let part1_result = part1 problem in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 problem in
  Printf.printf "Part 2: %d\n" part2_result
