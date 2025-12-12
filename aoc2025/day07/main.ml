type level = int list
type problem = { levels : level list; start : int }

let part1 problem : int = 0
let part2 problem : int = 0
let parse_problem (lines : string list) : problem = failwith ""

let () =
  let lines = Aoc2025.read_input_lines "./day07/input.txt" in

  let problem = parse_problem lines in

  let part1_result = part1 problem in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 problem in
  Printf.printf "Part 2: %d\n" part2_result
