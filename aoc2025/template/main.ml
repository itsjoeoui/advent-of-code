let part1 (input : string) : int =
  (* Placeholder implementation for part 1 *)
  String.length
    input (* Just returns the length of the input as a dummy result *)

let part2 (input : string) : int =
  (* Placeholder implementation for part 2 *)
  String.length
    input (* Just returns the length of the input as a dummy result *)

let () =
  let all = Aoc2025.read_input_all "./day00/input.txt" in

  let part1_result = part1 all in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 all in
  Printf.printf "Part 2: %d\n" part2_result
