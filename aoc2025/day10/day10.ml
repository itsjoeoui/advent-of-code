open Core

type problem = {
  target_mask : int;
  button_masks : int array;
  _joltages : int array;
}

let parse_problem (line : string) : problem =
  let items = String.split line ~on:' ' in
  let initial_parsed = (0, [], []) in

  let target_mask, button_masks_list, joltages_list =
    List.fold items ~init:initial_parsed ~f:(fun (sm, bml, jl) item ->
        match item.[0] with
        | '[' ->
            let state_str =
              String.sub item ~pos:1 ~len:(String.length item - 2)
            in
            let new_sm =
              String.foldi state_str ~init:0 ~f:(fun i acc char ->
                  if Char.equal char '#' then acc lor (1 lsl i) else acc)
            in
            (new_sm, bml, jl)
        | '(' ->
            let group =
              String.sub item ~pos:1 ~len:(String.length item - 2)
              |> String.split ~on:',' |> List.map ~f:int_of_string
            in
            let button_mask =
              List.fold group ~init:0 ~f:(fun acc light_idx ->
                  acc lor (1 lsl light_idx))
            in
            (sm, button_mask :: bml, jl)
        | '{' ->
            let new_jl =
              String.sub item ~pos:1 ~len:(String.length item - 2)
              |> String.split ~on:',' |> List.map ~f:int_of_string
            in
            (sm, bml, new_jl)
        | _ -> failwith ("Unknown item prefix: " ^ item))
  in
  {
    target_mask;
    button_masks = Array.of_list (List.rev button_masks_list);
    _joltages = Array.of_list joltages_list;
  }

let parse_problems (lines : string list) : problem array =
  lines |> List.map ~f:parse_problem |> Array.of_list

let solve_problem_part_1 (problem : problem) : int =
  let num_buttons = Array.length problem.button_masks in
  let target_mask = problem.target_mask in
  let button_masks = problem.button_masks in

  let rec dfs button_idx current_lights_mask current_presses =
    if button_idx = num_buttons then
      if current_lights_mask = target_mask then current_presses
      else Int.max_value
    else
      let result_without_pressing =
        dfs (button_idx + 1) current_lights_mask current_presses
      in

      let button_toggle_mask = button_masks.(button_idx) in
      let next_lights_mask = current_lights_mask lxor button_toggle_mask in

      let result_with_pressing =
        dfs (button_idx + 1) next_lights_mask (current_presses + 1)
      in

      min result_without_pressing result_with_pressing
  in

  dfs 0 0 0

let part1 (problems : problem array) : int =
  Array.fold problems ~init:0 ~f:(fun total_min_presses problem ->
      solve_problem_part_1 problem + total_min_presses)

let solve_problem_part_2 (problem : problem) : int =
  let ctx = Z3.mk_context [] in

  let opt = Z3.Optimize.mk_opt ctx in

  let x_vals =
    Array.init (Array.length problem.button_masks) ~f:(fun i ->
        Z3.Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "x_%d" i))
  in

  let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in

  Array.iter x_vals ~f:(fun x_val ->
      let cons_geq_zero = Z3.Arithmetic.mk_ge ctx x_val zero in
      Z3.Optimize.add opt [ cons_geq_zero ]);

  let b_vals =
    Array.init (Array.length problem._joltages) ~f:(fun i ->
        Z3.Arithmetic.Integer.mk_numeral_i ctx problem._joltages.(i))
  in

  Array.iteri b_vals ~f:(fun i b_val ->
      let relevant_buttons =
        Array.foldi problem.button_masks ~init:[] ~f:(fun j acc mask ->
            if (mask lsr i) land 1 = 1 then x_vals.(j) :: acc else acc)
      in
      let sum_expr = Z3.Arithmetic.mk_add ctx relevant_buttons in
      let b_eq = Z3.Boolean.mk_eq ctx sum_expr b_val in
      Z3.Optimize.add opt [ b_eq ]);

  let total_presses_expr = Z3.Arithmetic.mk_add ctx (Array.to_list x_vals) in

  let _ = Z3.Optimize.minimize opt total_presses_expr in

  match Z3.Optimize.check opt with
  | Z3.Solver.SATISFIABLE ->
      let model = Option.value_exn (Z3.Optimize.get_model opt) in
      let result_expr =
        Option.value_exn (Z3.Model.evaluate model total_presses_expr true)
      in
      Int.of_string (Z3.Expr.to_string result_expr)
  | _ -> failwith "No solution found for part 2"

let part2 (problems : problem array) : int =
  Array.fold problems ~init:0 ~f:(fun total_presses problem ->
      solve_problem_part_2 problem + total_presses)

let () =
  let lines = Aoc2025.read_input_lines "./day10/input.txt" in

  let problems = parse_problems lines in

  let part1_result = part1 problems in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 problems in
  Printf.printf "Part 2: %d\n" part2_result
