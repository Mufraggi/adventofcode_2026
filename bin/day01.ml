type direction = Right | Left


let direction_char c = match c with
  | 'R' -> Right
  | 'L' -> Left
  | _ -> failwith "Invalid operateur"



let iteration_operator dir = match dir with
  | Right -> 1
  | Left -> -1

let rec incremant_position start dir nb count =
  if nb = 0 then (start, count)
  else
    let delta = iteration_operator dir in
    let new_position = start + delta in
    
    let normalized_position = 
      if new_position >= 100 then new_position - 100
      else if new_position < 0 then new_position + 100
      else new_position
    in
    
    let new_count = if normalized_position = 0 then count + 1 else count in
    
    incremant_position normalized_position dir (nb - 1) new_count
  
  
let input = 
  let ic = open_in "inputs/day01.txt" in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec iterate array start count_zeros count = match array with
| [] -> count
| head::tail -> 
      let dir_char = String.get head 0 in
      let nb_str = String.sub head 1 (String.length head - 1) in
      
      let dir = direction_char dir_char in
      let nb = int_of_string nb_str in
      
      let (new_position, countNew) = incremant_position start dir nb 0 in
      
      (* Count if we landed on 0 *)
      let new_count = if new_position = 0 then count_zeros + 1 else count_zeros in
      
      iterate tail new_position new_count count+countNew

let input_moves = 
  input
  |> String.split_on_char '\n' 
  |> List.map (String.split_on_char ',') 
  |> List.flatten                    
  |> List.map String.trim            
  |> List.filter (fun s -> s <> "")


let password = iterate input_moves 50 0 0



let () = Printf.printf "Day 01 — part 1: %d\n" (password);
  

