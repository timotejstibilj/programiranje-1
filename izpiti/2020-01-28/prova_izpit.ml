let rec sum(x, y) = 
    match x, y with
        | Some a, Some b -> Some (a + b)
        | _, _ -> None

let rec two_step_map f g h x =
    match f x with
        | (a, b) -> (g a, h b)
    
(*functional repeat*)

let rec function_repeat f list =	
  let rec extender x acc n = 	
    if n > 0 then extender x (x::acc) (n-1) else acc 	
  in	
  let rec repeater acc = function	
    | [] -> List.rev acc	
    | x :: xs -> repeater (extender x acc (f x)) xs	
  in	
  repeater [] list

let rec iterate f condition x = 
    if condition x then x
    else iterate f condition (f x)
  

(*druga naloga*)

type 'a improved_list = 
  | Empty 
  | Node of 'a array * 'a improved_list

let testni_list = Node([|1.;2.;20.|], Node([|17.;19.;20.;30.|], Node([|100.|], Empty)))

let rec count = function
  | Empty -> 0
  | Node (array, list) -> 1 + count list

let rec count_inside = function
  | Empty -> 0
  | Node (array, list) -> (Array.length array) + count_inside list

let rec n_th_element n list =
  match list with
    | Empty -> None
    | Node (array, list2) -> 
      let length = Array.length array in
      if n < length then Some array.(n)
      else n_th_element (n - length) list2

let rec is_ordered improved_list = 
  let spodnja_meja = ref neg_infinity in
  let vrni = ref true in
  let rec pomozna tabela =
    for i = 0 to (Array.length tabela - 1) do
      if tabela.(i) < !spodnja_meja then vrni := false
      else spodnja_meja := tabela.(i)
    done;
    !vrni
  in
  match improved_list with
  | Empty -> true
  | Node (array, list) -> pomozna array && is_ordered list
  (* med arrayi ne gleda če se en začne z večjim kot se drugi konča *)


let rec update ilist i x = 	
  match ilist with 	
  | Empty -> Empty	
  | Node (arr, rest) -> 	
      if i >= Array.length arr then 	
        Node (arr, update rest (i - Array.length arr) x)	
      else	
        let arr' = Array.copy arr in	
        let _ = arr'.(i) <- x in	
        Node (arr', rest) 


let dot_prod (x1,y1,z1) (x2,y2,z2) = 	
  x1 *. x2 +. y1 *. y2 +. z1 *. z2 

let rec combine_and_filter f xs ys =
  let rec aux acc f xs ys=
    match xs, ys with
    | [], _ -> List.rev acc
    | _, [] -> List.rev acc
    | x :: restx, y :: resty ->
      let c = f x y in
      match c with
      | None -> aux acc f restx resty
      | Some c -> aux (c :: acc) f restx resty
  in

  aux [] f xs ys

type ('a, 'b) tree = 	
  | Empty 	
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree 	
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree 

let rec testno_drevo = ANode(BNode(Empty, true, Empty), 12, ANode(ANode(Empty, 0, Empty),5,BNode(Empty, false, Empty)))

let rec adepth = function
  | Empty -> 0
  | ANode(l, x, r) -> 1 + max (adepth l) (adepth r) 
  | BNode(l, x, r) -> 
    let kr = max (adepth l) (adepth r)  in
    if kr = 0 then 0 else kr + 1

type result = {anode : int; bnode : int}


