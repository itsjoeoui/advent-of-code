open Core

type coord = { x : int; y : int; z : int }

let index_combs_of_2 n =
  let rec loop i j acc =
    if i >= n then acc
    else if j >= n then loop (i + 1) (i + 2) acc
    else loop i (j + 1) ((i, j) :: acc)
  in
  loop 0 1 []

let parse_problem (lines : string list) : coord array =
  lines
  |> List.map ~f:(fun line ->
      Scanf.sscanf line "%d, %d, %d" (fun x y z -> { x; y; z }))
  |> Array.of_list

let dist_sq c1 c2 =
  let dx = c1.x - c2.x in
  let dy = c1.y - c2.y in
  let dz = c1.z - c2.z in
  (dx * dx) + (dy * dy) + (dz * dz)

let part1 (coords : coord array) : int =
  let limit = 1000 in

  let n = Array.length coords in

  let uf_nodes = Array.init n ~f:(fun i -> Union_find.create i) in

  let sorted_pairs_with_dist =
    index_combs_of_2 n
    |> List.map ~f:(fun (i, j) ->
        let d = dist_sq coords.(i) coords.(j) in
        (d, i, j))
    |> List.sort ~compare:(fun (d1, _, _) (d2, _, _) -> Int.compare d1 d2)
  in

  let top_pairs = List.take sorted_pairs_with_dist limit in

  List.iter top_pairs ~f:(fun (_dist, i, j) ->
      Union_find.union uf_nodes.(i) uf_nodes.(j));

  let group_counts = Int.Table.create () in

  Array.iter uf_nodes ~f:(fun node ->
      let root = Union_find.get node in
      Hashtbl.incr group_counts root);

  Hashtbl.data group_counts
  |> List.sort ~compare:Int.descending
  |> (fun l -> List.take l 3)
  |> List.fold ~init:1 ~f:( * )

let part2 (coords : coord array) : int =
  let n = Array.length coords in

  let uf_nodes = Array.init n ~f:(fun i -> Union_find.create i) in

  let sorted_pairs_with_dist =
    index_combs_of_2 n
    |> List.map ~f:(fun (i, j) ->
        let d = dist_sq coords.(i) coords.(j) in
        (d, i, j))
    |> List.sort ~compare:(fun (d1, _, _) (d2, _, _) -> Int.compare d1 d2)
  in

  let rec find_last_connection current_groups pairs =
    match pairs with
    | [] -> failwith "Ran out of pairs without connecting graph"
    | (_dist, i, j) :: rest ->
        let root_i = uf_nodes.(i) in
        let root_j = uf_nodes.(j) in

        if Union_find.same_class root_i root_j then
          find_last_connection current_groups rest
        else begin
          Union_find.union root_i root_j;
          let new_group_count = current_groups - 1 in

          if new_group_count = 1 then coords.(i).x * coords.(j).x
          else find_last_connection new_group_count rest
        end
  in

  find_last_connection n sorted_pairs_with_dist

let () =
  let lines = Aoc2025.read_input_lines "./day08/input.txt" in

  let coords = parse_problem lines in

  let part1_result = part1 coords in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 coords in
  Printf.printf "Part 2: %d\n" part2_result
