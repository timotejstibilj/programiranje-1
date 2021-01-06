("merge sort")

let rec razdeli_na_pol sez =
  match sez with
  | [] -> ([], [])
  | glava :: rep ->
      let (sodi_v_repu, lihi_v_repu) = razdeli_na_pol rep in
      (glava :: lihi_v_repu, sodi_v_repu)

    
let rec zlij sez1 sez2 = 
  match sez1, sez2 with
  | [], [] -> []
  | ([], _) -> sez2 
  | (_, []) -> sez1
  | x :: xs, y :: ys ->
    if x < y then x :: zlij xs sez2
    else y :: zlij sez1 ys

let rec merge_sort sez =
  match sez with
  | [] -> []
  | [x] -> [x]
  | sez ->
      let (sez1, sez2) = razdeli_na_pol sez in
      let sez1' = merge_sort sez1
      and sez2' = merge_sort sez2 in
      zlij sez1' sez2'

(*quick sort*)

(* razdeli na dva seznama *)
let rec pivotiraj pivot =
  function
  | [] -> ([], [])
  | glava :: rep ->
      let (manjsi, vecji) = pivotiraj pivot rep in
      if glava < pivot then
        (glava :: manjsi, vecji)
      else
        (manjsi, glava :: vecji)

let seznam = [2;3;1;5;4;6]


let rec hitro_uredi =
  function
  | [] -> []
  | glava :: rep ->
     let (sez1, sez2) = pivotiraj glava rep in
      let sez1' = hitro_uredi sez1
      and sez2' = hitro_uredi sez2 in
      sez1' @ glava :: sez2'


