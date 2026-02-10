open Ast
open Sysstate
open Utils

(* Questa funzione ci permette di restituire la mutabilità della funzione*)
let get_mut_from_decl = function
  | (Constr(_,_,m)) -> m
  | (Proc(_,_,_,_,m,_)) -> m
;;

let is_mut_view = function
  | View -> true
  | _ -> false
;;

let is_mut_pure = function
  | Pure -> true
  | _ -> false
;;

let is_fun_view (fdecl : fun_decl) =
  is_mut_view (get_mut_from_decl fdecl)
;;

let is_fun_pure_or_view (fdecl : fun_decl) =
  let mut = get_mut_from_decl fdecl in
  is_mut_pure mut || is_mut_view mut
;;

(*Prende il valore della mutabilità dalla variabile salvata nell'ultimo frame dello stack*)
let lookup_mut_view st =
  let fr = List.hd st.callstack in
  match lookup_locals "mut_view" fr.locals with
  | Some v -> bool_of_expr (expr_of_exprval v)
  | None -> false
;;

let update_var_wrapper st x v=
  if lookup_mut_view st then
    try 
      let fr = List.hd st.callstack in
      let fr' = { fr with locals = update_locals fr.locals x v } in
      St({ st with callstack = fr' :: (List.tl st.callstack) })
    with _ -> Reverted("Pu")
  else
    St(update_var st x v);;
  ;;

(*update_var (st : sysstate) (x : ide) (v : exprval) : sysstate = 
  let fr = List.hd st.callstack in

  (* first tries to update environment if x is bound there *)
  try 
    let fr' = { fr with locals = update_locals fr.locals x v } in
    { st with callstack = fr' :: (List.tl st.callstack)  }
  with _ -> 
    (* if not, tries to update storage of a *)
    let cs = st.accounts fr.callee in
    if exists_ide_in_storage cs x then 
      let cs' = { cs with storage = bind x v cs.storage } in 
      { st with accounts = bind fr.callee cs' st.accounts }
    else failwith (x ^ " not bound in storage of " ^ fr.callee)   *)