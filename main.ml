open Printf

let rec last list = 
    match list with
    | [] -> None
    | [x] -> Some x
    | hd :: tl -> last tl
;;

let rec last_two list =
    match list with
    | [] -> None
    | [x] -> None
    | x :: [y] -> Some (x, y)
    | hd :: tl -> last_two (tl)
;;

let rec nth list idx =
    match (idx, list) with
    | _, [] -> None
    | 0, hd :: tl -> Some hd 
    | _, hd :: tl -> nth tl (idx -1)
;;
    

let rec length list =
    let rec aux list count =
        match (list, count) with
        | [], x -> x
        | hd :: tl, _ -> aux tl (count + 1)
    in 
    aux list 0
;;

let rec reverse list =
    match list with
    | [] -> []
    | hd :: tl -> (reverse tl) @ [hd]
;;

let is_palindrome i = 
    (reverse i) = i
;;

type 'a node =
    | One of 'a
    | Many of 'a node list
;;

let rec flatten lst =
    let rec aux lst out_lst =
        match lst with
        | One hd :: tl -> aux tl (hd :: out_lst)
        | Many hd :: tl -> aux tl ((aux hd []) @ out_lst)
        | [] -> out_lst
    in
    reverse (aux lst [])
;;

let rec compress lst =
    let rec aux lst out =
        match (lst, out) with
        | [], _ -> out
        | hd :: tl, out_hd :: _ 
            when hd = out_hd -> aux tl (out)
        | hd :: tl, _ -> aux tl (hd :: out)
    in 
    reverse (aux lst [])
;;

let rec pack lst =
    let rec aux lst out =
        match (lst, out) with
        | [], _ -> out
        | hd :: tl, out_hd :: out_tl 
            when hd = List.hd out_hd -> aux tl ([[hd] @ out_hd] @ out_tl)
        | hd :: tl, _ -> aux tl ([hd] :: out)
    in
    reverse (aux lst [])
;;

let rec encode lst =
    let rec aux lst out =
        match (lst, out) with
        | [], _ -> out
        | hd :: tl, (hd_cnt, hd_val) :: out_tl 
            when hd = hd_val -> aux tl ((hd_cnt + 1, hd_val) :: out_tl)
        | hd :: tl, _ -> aux tl ((1, hd) :: out)
    in 
    reverse (aux lst [])
;;

type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let rec encode lst =
    let rec aux lst out =
        match (lst, out) with
        | [], _ -> out
        | hd :: tl, Many (hd_cnt, hd_val) :: out_tl 
            when hd = hd_val -> aux tl (Many (hd_cnt + 1, hd_val) :: out_tl)
        | hd :: tl, One hd_val :: out_tl
            when hd = hd_val -> aux tl (Many (2, hd_val) :: out_tl)
        | hd :: tl, _ -> aux tl (One hd :: out)
    in 
    reverse (aux lst [])
;;

let rec decode lst =
    let rec list_of_size size default_val out =
        match size with
        | 0 -> out
        | n -> list_of_size (n-1) default_val ([default_val] @ out)
    in
    let rec aux lst out =
        match lst with
        | [] -> out
        | One n :: tl -> aux tl (n :: out)
        | Many (size, v) :: tl -> aux tl ((list_of_size size v []) @ out)
    in
    reverse (aux lst [])
;;

let rec duplicate lst =
    let rec aux lst out =
        match lst with
        | [] -> out
        | hd :: tl -> aux tl (hd :: hd :: out)
    in
    reverse (aux lst [])
;;

let rec replicate lst n =
    let rec push_n lst item n =
        match n with
        | 0 -> lst
        | x -> push_n (item :: lst) item (n - 1)
    in
    let rec aux lst out =
        match lst with
        | [] -> out
        | hd :: tl -> aux tl (push_n out hd n)
    in
    reverse (aux lst [])
;;

let rec drop list n =
    let should_drop out_lst =
        (((length out_lst)+1) mod n) = 0
    in
    let rec aux lst out =
        match lst with
        | [] -> out
        | hd :: tl when (should_drop lst) -> aux tl out
        | hd :: tl -> aux tl (hd::out)
    in
    reverse (aux list [])
;;

let rec split lst idx =
    let rec aux lst left right i =
        match lst with
        | [] -> 
            if idx >= 0 then 
                (reverse left, reverse right)
            else
                (right, left)
        | hd :: tl when (length left) < i -> aux tl (hd :: left) right i
        | hd :: tl -> aux tl left (hd :: right) i
    in
    if idx >= 0 then
        aux lst [] [] idx
    else
        aux (reverse lst) [] [] (-idx)
;;

let rec slice lst start stop =
    let diff = stop - start + 1 in
    let _, right = split lst start in
    let rec aux input out idx = 
        match idx, input with
        | 0, _ -> out
        | _, [] -> out
        | n, hd :: tl -> aux tl (hd :: out) (idx - 1)
    in
    reverse (aux right [] diff)
;;

let rotate lst idx =
    let split_list = split lst idx in
    match split_list with
    | (x, y) -> y @ x
;;

let rec remove_at idx lst =
    match idx, lst with
    | _, [] -> []
    | 0, hd :: tl -> tl
    | x, hd :: tl -> hd :: (remove_at (idx-1) tl)
;;

let rec insert_at value idx lst =
    match idx, lst with
    | _, [] -> [value]
    | 0, hd :: tl -> value :: hd :: tl
    | x, hd :: tl -> hd :: (insert_at value (idx-1) tl)
;;

let rec range start stop =
    if (start = stop) then
        [start]
    else if (start < stop) then
        start :: (range (start + 1) stop)
    else if (start > stop) then
        start :: (range (start - 1) stop)
    else
        []
;;

let rec rand_select lst n =
    let l = (length lst) in
    if (l > n) then
        rand_select (remove_at (Random.int l) lst) n
    else
        lst
;;

let lotto_select n m =
    let set = range 1 m in
    rand_select set n
;;

let rec rand_permute lst =
    let rec aux inp out =
        match inp with
        | [] -> out
        | _ ->
            let len = (length inp) in
            let selection_idx = Random.int len in
            let selection_val = List.nth inp selection_idx in
            aux (remove_at selection_idx inp) (selection_val :: out)
    in
    aux lst []
;;

let rec extract n lst =
    match n, lst with
    | 0, _ -> [[]]
    | _, [] -> []
    | _, hd :: tl ->
        let without_hd = (extract n tl) in
        let with_hd = List.map (fun l -> hd :: l) (extract (n-1) tl) in
        with_hd @ without_hd
;;

(* TODO: Problem 27 *)

let rec longest lst =
    let rec aux inp cur_len cur_out cur_idx =
        match inp with
        | [] -> (cur_idx, cur_out)
        | hd :: tl ->
            if (length hd) > cur_len then
                aux tl (length hd) hd ((length lst) - (length inp))
            else
                aux tl cur_len cur_out cur_idx
    in
    aux lst 0 [] (-1)
;;    

let rec length_sort lst =
    let rec aux inp out =
        match inp with
        | [] -> out 
        | _ -> 
            let idx, longest_sublist = longest inp in
            aux (remove_at idx inp) (longest_sublist :: out)
    in
    aux lst []
;;

let rec lengths lst = 
    let rec aux inp out =
        match inp with
        | [] -> out
        | hd :: tl -> aux tl ((length hd) :: out)
    in 
    aux lst []
;;

let rec biggest lst =
    let rec aux inp cur_out cur_idx =
        match inp with
        | [] -> (cur_idx, cur_out)
        | hd :: tl -> 
            if hd > cur_out then
                (biggest tl hd ((length lst) - (length inp)))
            else
                (biggest tl cur_out cur_idx)
    in
    aux (lst) (Int.min_int) (-1)
;;

let rec int_sort lst =
    let rec sort inp out =
        match inp with
        | [] -> out
        | _ -> 
            let idx, largest = (biggest inp) in
            printf "%d %d\n" idx largest;
            List.iter (printf "%d ") inp;
            print_endline;
            sort (remove_at idx inp) (largest :: out)
    in
    sort lst []
;;
