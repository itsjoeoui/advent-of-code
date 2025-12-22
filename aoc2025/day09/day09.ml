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

let part2 (coords : coord array) : int =
  (* coordinate compression*)
  let build_coord_comp_map raw_values =
    let sorted =
      raw_values |> Array.to_list |> List.dedup_and_sort ~compare:Int.compare
    in

    let map, last_idx, _ =
      (* start at index 1 to leave padding for index 0 *)
      List.fold sorted ~init:(Int.Map.empty, 1, None)
        ~f:(fun (acc_map, prev_idx, prev_val) curr_val ->
          let step =
            match prev_val with
            | Some v when v = curr_val - 1 -> 1
            | Some _ -> 2
            | None -> 0
          in
          let new_idx = prev_idx + step in
          ( Map.add_exn acc_map ~key:curr_val ~data:new_idx,
            new_idx,
            Some curr_val ))
    in

    (* add 1 for last padding, and since it wants overall size we add another 1 *)
    (map, last_idx + 2)
  in

  let map_x, width =
    build_coord_comp_map (Array.map coords ~f:(fun c -> c.x))
  in
  let map_y, height =
    build_coord_comp_map (Array.map coords ~f:(fun c -> c.y))
  in

  (* fill wall boundary *)
  let is_wall = Array.make_matrix ~dimx:width ~dimy:height false in

  let fill_rect x1 y1 x2 y2 =
    let rec loop_x x =
      if x > x2 then ()
      else
        let rec loop_y y =
          if y > y2 then ()
          else (
            is_wall.(x).(y) <- true;
            loop_y (y + 1))
        in
        loop_y y1;
        loop_x (x + 1)
    in
    loop_x x1
  in

  let l_coords = Array.to_list coords in
  let edges =
    List.zip_exn l_coords (List.tl_exn l_coords @ [ List.hd_exn l_coords ])
  in

  List.iter edges ~f:(fun (p1, p2) ->
      let x1, y1 = (Map.find_exn map_x p1.x, Map.find_exn map_y p1.y) in
      let x2, y2 = (Map.find_exn map_x p2.x, Map.find_exn map_y p2.y) in
      fill_rect (Int.min x1 x2) (Int.min y1 y2) (Int.max x1 x2) (Int.max y1 y2));

  (* floor fill outside of our shape *)
  let is_outside = Array.make_matrix ~dimx:width ~dimy:height false in
  let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in

  let rec flood_fill queue =
    match queue with
    | [] -> ()
    | (cx, cy) :: rest ->
        let neighbors =
          List.filter_map dirs ~f:(fun (dx, dy) ->
              let nx, ny = (cx + dx, cy + dy) in
              if
                nx >= 0 && nx < width && ny >= 0 && ny < height
                && (not is_wall.(nx).(ny))
                && not is_outside.(nx).(ny)
              then (
                is_outside.(nx).(ny) <- true;
                Some (nx, ny))
              else None)
        in
        flood_fill (rest @ neighbors)
  in

  is_outside.(0).(0) <- true;
  flood_fill [ (0, 0) ];

  (* prefix sum *)
  let psum = Array.make_matrix ~dimx:(width + 1) ~dimy:(height + 1) 0 in
  let rec build_psum x y =
    if x >= width then ()
    else if y >= height then build_psum (x + 1) 0
    else (
      psum.(x + 1).(y + 1) <-
        (if is_outside.(x).(y) then 1 else 0)
        + psum.(x).(y + 1)
        + psum.(x + 1).(y)
        - psum.(x).(y);
      build_psum x (y + 1))
  in
  build_psum 0 0;

  (* points in our compressed coord system *)
  let points =
    Array.map coords ~f:(fun c ->
        (c, Map.find_exn map_x c.x, Map.find_exn map_y c.y))
  in
  let n = Array.length coords in

  (* generate all pairs, calculating area first *)
  index_combs_of_2 n
  |> List.map ~f:(fun (i, j) ->
      let c1, _, _ = points.(i) in
      let c2, _, _ = points.(j) in
      (area c1 c2, i, j))
  (* sort descending by area *)
  |> List.sort ~compare:(fun (a1, _, _) (a2, _, _) -> Int.compare a2 a1)
  (* find the first valid one *)
  |> List.find_map ~f:(fun (a, i, j) ->
      let _, cx1, cy1 = points.(i) in
      let _, cx2, cy2 = points.(j) in

      let x_min, x_max = (Int.min cx1 cx2, Int.max cx1 cx2) in
      let y_min, y_max = (Int.min cy1 cy2, Int.max cy1 cy2) in

      (* use the prefix sum table  *)
      let outside_sum =
        psum.(x_max + 1).(y_max + 1)
        - psum.(x_min).(y_max + 1)
        - psum.(x_max + 1).(y_min)
        + psum.(x_min).(y_min)
      in

      if outside_sum = 0 then Some a else None)
  |> Option.value_exn

let () =
  let lines = Aoc2025.read_input_lines "./day09/input.txt" in
  let coords = parse_problem lines in

  Printf.printf "Part 1: %d\n" (part1 coords);
  Printf.printf "Part 2: %d\n" (part2 coords)
