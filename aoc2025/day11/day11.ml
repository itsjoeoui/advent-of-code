open Core

type line = { from : string; tos : string list }

let parse_line line =
  line |> String.split ~on:':' |> function
  | [ x; ys ] ->
      let from = String.strip x in
      let tos =
        ys |> String.strip |> String.split ~on:' ' |> List.map ~f:String.strip
      in
      { from; tos }
  | _ -> failwith "Invalid line format"

let parse_graph lines =
  let graph = String.Table.create () in

  lines
  |> List.iter ~f:(fun line ->
      let result = parse_line line in
      Hashtbl.set graph ~key:result.from ~data:result.tos);

  graph

let count_paths graph start_node end_node =
  let memo = String.Table.create () in

  let rec aux node =
    match Hashtbl.find memo node with
    | Some count -> count
    | None ->
        let count =
          if String.equal node end_node then 1
          else
            match Hashtbl.find graph node with
            | None -> 0
            | Some neighbors -> List.sum (module Int) neighbors ~f:aux
        in
        Hashtbl.set memo ~key:node ~data:count;
        count
  in
  aux start_node

let part1 lines =
  let graph = parse_graph lines in
  count_paths graph "you" "out"

let part2 lines =
  let graph = parse_graph lines in

  let paths_svr_dac = count_paths graph "svr" "dac" in
  let paths_dac_fft = count_paths graph "dac" "fft" in
  let paths_fft_out = count_paths graph "fft" "out" in
  let route_dac_first = paths_svr_dac * paths_dac_fft * paths_fft_out in

  let paths_svr_fft = count_paths graph "svr" "fft" in
  let paths_fft_dac = count_paths graph "fft" "dac" in
  let paths_dac_out = count_paths graph "dac" "out" in
  let route_fft_first = paths_svr_fft * paths_fft_dac * paths_dac_out in

  route_dac_first + route_fft_first

let () =
  let lines = Aoc2025.read_input_lines "./day11/input.txt" in

  let part1_result = part1 lines in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 lines in
  Printf.printf "Part 2: %d\n" part2_result
