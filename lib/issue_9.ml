open Ast

let rec find_fun_in_fdl (fdl : fun_decl list) (f : ide) : fun_decl option = 
  match fdl with
  | [] -> None 
  | Proc(fn, lv, c , v , m , rtl) :: _ when f = fn -> Some(Proc(fn, lv, c , v , m , rtl))
  | _ :: t -> find_fun_in_fdl t f 
;;

let basetype_of_vartype vt = 
  match vt with
  | VarT (t) -> t
  | MapT (_,_) -> failwith "impossible to cast a mapping to exprtype"
;;

