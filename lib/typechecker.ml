open Ast
open Utils
open Prettyprint

(* Types for expressions are a refinement of variable declaration types, 
 * since we want to give more specific types to integer constants in order
 * to have a smoother treatment of int and uint types 
 *)
type exprtype = 
  | BoolConstET of bool
  | BoolET
  | IntConstET of int
  | IntET
  | UintET
  | AddrET of bool
  | EnumET of ide
  | ContractET of ide
  | MapET of exprtype * exprtype

let rec string_of_exprtype = function
  | BoolConstET b   -> "bool " ^ (if b then "true" else "false")
  | BoolET          -> "bool"
  | IntConstET n    -> "int " ^ string_of_int n
  | IntET           -> "int"
  | UintET          -> "uint"
  | AddrET p        -> "address" ^ (if p then " payable" else "")
  | EnumET x        -> x
  | ContractET x    -> x
  | MapET(t1,t2)    -> string_of_exprtype t1 ^ " => " ^ string_of_exprtype t2

(* the result of the contract typechecker is either:
  - Ok():       if all static checks passed
  - Error log:  if some checks did not pass; the log collects all the errors found 
 *)
type typecheck_result = (unit,exn list) result

(* >> merges two contract typechecker results *)
let (>>)  (out1 : typecheck_result) (out2 : typecheck_result) : typecheck_result =
  match out1 with
  | Ok () -> out2
  | Error log1 -> match out2 with 
    | Ok () -> Error log1 
    | Error log2 -> Error (log1 @ log2)

(* the result of the expression typechecker is either:
  - Ok(t):      if all static checks passed, and t is the inferred type of the expression
  - Error log:  if some checks did not pass; the log collects all the errors found 
 *)

type typecheck_expr_result = (exprtype,exn list) result

(* >>+ merges two expression typechecker results *)
let  (>>+)  (out1 : typecheck_expr_result) (out2 : typecheck_expr_result) : typecheck_expr_result =
  match out1,out2 with
  | Ok _,Ok _ -> assert(false) (* should not happen *)
  | Ok _, Error log2 -> Error log2
  | Error log1, Ok _ -> Error log1 
  | Error log1,Error log2 -> Error (log1 @ log2)

(* boring cast from expression typechecker result to contract typechecker result*)
let typecheck_result_from_expr_result (out : typecheck_expr_result) : typecheck_result =
  match out with
  | Error log -> Error log
  | Ok(_) -> Ok()

(* The following exceptions represent the all possible errors detected by the typechecker *)
exception TypeError of ide * expr * exprtype * exprtype
exception NotMapError of ide * expr
exception ImmutabilityError of ide * ide
exception UndeclaredVar of ide * ide
exception MultipleDecl of ide
exception MultipleLocalDecl of ide * ide
exception EnumNameNotFound of ide * ide
exception EnumOptionNotFound of ide * ide * ide
exception EnumDupName of ide
exception EnumDupOption of ide * ide
exception MapInLocalDecl of ide * ide
exception MultipleReturnTypes of ide
exception MissingReturnType of ide
exception MissingReturnStatement of ide
exception ParameterMismatch of ide * ide
exception NegativeTransferValue of ide
exception FunctionNotFound of ide * ide
exception NonPayableTransfer of ide * expr
exception UnknownReturnType of ide * ide * expr 
let logfun f s = "(" ^ f ^ ")\t" ^ s 

(* Prettyprinting of typechecker errors *)
let string_of_typecheck_error = function
| TypeError (f,e,t1,t2) -> 
    logfun f
    "expression " ^ (string_of_expr e) ^ 
    " has type " ^ string_of_exprtype t1 ^
    " but is expected to have type " ^ string_of_exprtype t2
| NotMapError (f,e) -> logfun f (string_of_expr e) ^ " is not a mapping"
| ImmutabilityError (f,x) -> logfun f "variable " ^ x ^ " was declared as immutable, but is used as mutable"
| UndeclaredVar (f,x) -> logfun f "variable " ^ x ^ " is not declared"
| MultipleDecl x -> "variable " ^ x ^ " is declared multiple times"
| MultipleLocalDecl (f,x) -> logfun f "variable " ^ x ^ " is declared multiple times"
| EnumNameNotFound (f,x) -> logfun f "enum ^ " ^ x ^ " is not declared"
| EnumOptionNotFound (f,x,o) -> logfun f "enum option " ^ o ^ " is not found in enum " ^ x
| EnumDupName x -> "enum " ^ x ^ " is declared multiple times"
| EnumDupOption (x,o) -> "enum option " ^ o ^ " is declared multiple times in enum " ^ x
| MapInLocalDecl (f,x) -> logfun f "mapping " ^ x ^ " not admitted in local declaration"
| MultipleReturnTypes(f) -> logfun f "multiple return types are not admitted"
| MissingReturnType(f) -> logfun f "function should have a return type"
| MissingReturnStatement(f) -> logfun f "function declares a return type but has no return statement"
| ParameterMismatch(f, cf) -> logfun f "number of argument calling function " ^ cf ^ " does not match function arity"
| NegativeTransferValue(f) -> logfun f "it is not possible to transfer a negative amount of wei"
| FunctionNotFound(f, cf) -> logfun f "can't find local function" ^ cf ^ "in contract"
| NonPayableTransfer(f, a) -> logfun f "address " ^ (string_of_expr a)^ " is not payable"
| UnknownReturnType(f, cf, a) -> logfun f "return type of function " ^ (string_of_expr a) ^ "." ^ cf ^ " is unknown"
| ex -> Printexc.to_string ex

let exprtype_of_decltype = function
  | IntBT         -> IntET
  | UintBT        -> UintET
  | BoolBT        -> BoolET
  | AddrBT(b)     -> AddrET(b)
  | EnumBT _      -> UintET
  | ContractBT x  -> ContractET x 
  | UnknownBT _   -> assert(false) (* should not happen after preprocessing *)

(* typechecker functions take as input the list of variable declarations:
  - var_decl:       state variables 
  - local_var_decl: local variables
  The type all_var_decls encapsulates the list of these variables.
*)

type all_var_decls = (var_decl list) * (local_var_decl list)

let get_state_var_decls (avdl : all_var_decls) : var_decl list = fst avdl 
let get_local_var_decls (avdl : all_var_decls) : local_var_decl list = snd avdl 

(* merges a list of state variable decls and a list of local variable decls *)
let merge_var_decls (vdl : var_decl list) (lvdl : local_var_decl list) : all_var_decls = vdl , lvdl  

(* adds a list of local variables to all_var_decls *)
let push_local_decls ((vdl: var_decl list),(old_lvdl : local_var_decl list)) new_lvdl = 
  (vdl , new_lvdl @ old_lvdl)  

let lookup_type (x : ide) (avdl : all_var_decls) : exprtype option =
  if x="msg.sender" then Some (AddrET false)
  else if x="msg.value" then Some UintET else
  (* first lookup the local variables *)
  avdl 
  |> get_local_var_decls 
  |> List.map (fun (vd : local_var_decl) -> match vd.ty with
    | VarT(t)   -> (exprtype_of_decltype t),vd.name 
    | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),vd.name)
  |> List.fold_left
  (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
  None
  |>
  fun res -> match res with
    | Some t -> Some t
    | None -> (* if not found, lookup the state variables *)
      avdl 
      |> get_state_var_decls  
      |> List.map (fun (vd : var_decl) -> match vd.ty with
        | VarT(t)   -> (exprtype_of_decltype t),vd.name 
        | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),vd.name)
      |> List.fold_left
      (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
      None

let rec dup = function 
  | [] -> None
  | x::l -> if List.mem x l then Some x else dup l

(* no_dup_var_decls:
    checks that no variables are declared multiple times
 *)
let no_dup_var_decls vdl = 
  vdl 
  |> List.map (fun (vd : var_decl) -> vd.name) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleDecl x])  

let no_dup_local_var_decls f vdl = 
  vdl 
  |> List.map (fun (vd : local_var_decl) -> vd.name) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleLocalDecl (f,x)])  

let no_dup_fun_decls vdl = 
  vdl 
  |> List.map (fun fd -> match fd with 
    | Constr(_) -> "constructor"
    | Proc(f,_,_,_,_,_) -> f) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleDecl x])  
  
let subtype t0 t1 = match t1 with
  | BoolConstET _ -> (match t0 with BoolConstET _ -> true | _ -> false) 
  | BoolET -> (match t0 with BoolConstET _ | BoolET -> true | _ -> false) 
  | IntConstET _ -> (match t0 with IntConstET _ -> true | _ -> false)
  | UintET -> (match t0 with IntConstET n when n>=0 -> true | UintET -> true | _ -> false)
  | IntET -> (match t0 with IntConstET _ | IntET -> true | _ -> false) (* uint is not convertible to int *)
  | AddrET _ -> (match t0 with AddrET _ -> true | _ -> false)
  | _ -> t0 = t1
   
(******************** Helper per issue_9 ***********************)
(*controlla che tutti i percorsi di esecuzione abbiano un return*)
let rec all_paths_return = function
  | Seq(c1, c2) -> all_paths_return c1 || all_paths_return c2
  | If(_, c1, c2) -> all_paths_return c1 && all_paths_return c2
  | Block(_, c)
  | ExecBlock c
  | ExecProcCall c -> all_paths_return c
  | Return _ -> true
  | _ -> false

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

let get_al_rt (fdl : fun_decl list) (cf : ide) =
  match find_fun_in_fdl fdl cf with 
  | None -> None
  | Some(fd) -> (
    match fd with
    | Proc(_, al, _, _, _, rtl) -> Some(al, rtl)
    | Constr(_) -> failwith "Can't call constructor"
  )
;;

let check_value_non_payable f e_to e_value =
  match e_value with
  | IntConst 0 -> Ok(UintET)
  | _ -> Error[NonPayableTransfer (f, e_to)]

(*******)
let rec typecheck_expr (f : ide) (edl : enum_decl list) vdl (fdl : fun_decl list) = function
  | BoolConst b -> Ok (BoolConstET b)

  | IntConst n -> Ok (IntConstET n)

  | IntVal _ | UintVal _ -> assert(false) (* these expressions only occur at runtime *)

  | AddrConst _ -> Ok (AddrET false)

  | BlockNum -> Ok(UintET)

  | This -> Ok(AddrET false) (* TODO: check coherence with Solidity *)

  | Var x -> (match lookup_type x vdl with
    | Some t -> Ok(t)
    | None -> Error [UndeclaredVar (f,x)])

  | MapR(e1,e2) -> (match (typecheck_expr f edl vdl fdl e1, typecheck_expr f edl vdl fdl e2) with
    | Ok(MapET(t1k,t1v)),Ok(t2) when t2 = t1k -> Ok(t1v) 
    | Ok(MapET(t1k,_)),Ok(t2) -> Error [TypeError (f,e2,t2,t1k)]
    | _ -> Error [NotMapError(f,e1)]
    )

  | BalanceOf(e) -> (match typecheck_expr f edl vdl fdl e with
        Ok(AddrET(_)) -> Ok(UintET)
      | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
      | _ as err -> err)

  | Not(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(BoolConstET b) -> Ok(BoolConstET (not b))
      | Ok(BoolET) -> Ok(BoolET)
      | Ok(t) -> Error [TypeError (f,e,t,BoolET)]
      | _ as err -> err)

  | And(e1,e2) -> 
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(BoolConstET false),Ok(t2) when subtype t2 BoolET -> Ok(BoolConstET false)
     | Ok(t1),Ok(BoolConstET false) when subtype t1 BoolET -> Ok(BoolConstET false)
     | Ok(t1),Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1),_ when not (subtype t1 BoolET) -> Error [TypeError (f,e1,t1,BoolET)]
     | _,Ok(t) -> Error [TypeError (f,e2,t,BoolET)]
     | err1,err2 -> err1 >>+ err2)

  | Or(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(BoolConstET true),Ok(t2) when subtype t2 BoolET -> Ok(BoolConstET true)
     | Ok(t1),Ok(BoolConstET true) when subtype t1 BoolET -> Ok(BoolConstET true)
     | Ok(t1),Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1),_ when not (subtype t1 BoolET) -> Error [TypeError (f,e1,t1,BoolET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,BoolET)]
     | err1,err2 -> err1 >>+ err2)

  | Add(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1+n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Sub(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1-n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Mul(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1*n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Div(_) -> failwith "Div: TODO"

  | Eq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 = n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 -> Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Neq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <> n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 -> Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Leq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Lt(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 < n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)
    
  | Geq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 >= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Gt(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 > n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | IfE(e1,e2,e3) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2, typecheck_expr f edl vdl fdl e3) with
     | Ok(BoolConstET true),Ok(t2),_ -> Ok(t2)
     | Ok(BoolConstET false),_,Ok(t3) -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t2 t3 -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t3 t2 -> Ok(t2)
     | Ok(BoolET),Ok(t2),Ok(t3) -> Error [TypeError (f,e3,t3,t2)]
     | Ok(t1),_,_ -> Error [TypeError (f,e1,t1,BoolET)]
     | err1,err2,err3 -> err1 >>+ err2 >>+ err3)

  | IntCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET _) | Ok(IntET) | Ok(UintET) -> Ok(IntET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | UintCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET n) when n>=0 -> Ok(IntConstET n) 
      | Ok(IntET) | Ok(UintET) -> Ok(UintET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | AddrCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET(b))     -> Ok(AddrET b)
      | Ok(IntConstET _)  -> Ok(AddrET false) 
      | Ok(UintET)        -> Ok(AddrET false)
      | Ok(IntET)         -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)] 
      | err               -> err)

  | PayableCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET _)      -> Ok(AddrET true)
      | Ok(IntConstET 0)  -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)]
      | err               -> err)

  | EnumOpt(enum_name,option_name) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> Error [EnumNameNotFound (f,enum_name)]
        | Some ol -> (match find_index (fun o -> o=option_name) ol with
          None -> Error [EnumOptionNotFound(f,enum_name,option_name)]
          | Some i -> Ok(IntConstET i)))

  | EnumCast(x,e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET _) | Ok(UintET) | Ok(IntET) -> Ok(EnumET x)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | ContractCast(x,e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET _) -> Ok(ContractET x)
      | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
      | err -> err)

  | UnknownCast(_) -> assert(false) (* should not happen after preprocessing *)
  
  | FunCall(e_to, cf, e_value, e_args) -> (
    (*controllo che i tipi delle liste dei parametri passati e formali corrispondano*)
    let rec check_args (l1 : local_var_decl list) (l2 : expr list) : typecheck_result =
      match (l1,l2) with
      | [],[] -> Ok()
      | (h1::t1, h2::t2) ->
        let ty1 = exprtype_of_decltype (basetype_of_vartype (h1.ty)) in (
        match typecheck_expr f edl vdl fdl h2 with
        | Ok(ty2) when subtype ty2 ty1 -> check_args t1 t2
        | Ok(ty2) -> Error[TypeError (f, h2, ty2, ty1)]
        | err -> typecheck_result_from_expr_result err
        )
      | _ -> Error[ParameterMismatch (f, cf)]
    in
    
    (*controllo che il valore di wei trasferiti sia di tipo compatibile con UintET *)
    let check_value : typecheck_expr_result =
      match typecheck_expr f edl vdl fdl e_value with
      | Ok(t) when subtype t UintET -> Ok(UintET)
      | Ok(t) -> Error[TypeError (f, e_value, t, UintET)]
      | err -> err
    in

    (*controllo che il tipo di ritorno sia uno e uno soltanto e lo restituisce*)
    let check_ret rt : typecheck_expr_result =
      match rt with
      | [] -> Error [MissingReturnType cf]
      | [rt1] -> Ok(exprtype_of_decltype rt1)
      | _ -> Error [MultipleReturnTypes cf]
    in
    
    (*cerco la funzione nella lista di funzioni, se non esiste restituisce errore 
    altrimenti confronta i parametri formali con quelli effettivi e fa il check del tipo di ritorno, 
    se è tutto a posto lo restituisce altrimenti da errore*)
    let typecheck_fun_internal _fdl _cf: typecheck_expr_result =
      match get_al_rt _fdl _cf with
      | Some(al,rt) -> (
        match check_args al e_args, check_ret rt with
        | Ok(), Ok(t) -> Ok(t)
        | Ok(), Error err2 -> Error err2
        | Error err1, err2 -> Error err1 >>+ err2
        )
      | None -> Error [FunctionNotFound (f,_cf)]
    in
    
    match typecheck_expr f edl vdl fdl e_to with
    | Ok(AddrET true) -> (*Payable*) check_value >>+ Error[UnknownReturnType (f, cf, e_to)]
    | Ok(AddrET false) -> (*Non payable*) (
      match e_to, (check_value_non_payable f e_to e_value) with
      | This, Ok(UintET) -> typecheck_fun_internal fdl cf
      | _, err -> Error[UnknownReturnType (f, cf, e_to)] >>+ err (* Ignores calls to functions in other contracts *)
      )
    | Ok(t) -> Error[TypeError (f, e_to, t, AddrET false)]
    | err -> err

  )
  
  | ExecFunCall(_) -> assert(false) (* this should not happen at static time *)

let is_immutable (x : ide) (vdl : var_decl list) = 
  List.fold_left (fun acc (vd : var_decl) -> acc || (vd.name=x && vd.mutability<>Mutable)) false vdl

let typecheck_local_decls (f : ide) (vdl : local_var_decl list) = List.fold_left
  (fun acc vd -> match vd.ty with 
    | MapT(_) -> acc >> Error [MapInLocalDecl (f,vd.name)]
    | _ -> acc)
  (Ok ())
  vdl

let rec typecheck_cmd (f : ide) (edl : enum_decl list) (vdl : all_var_decls) (fdl : fun_decl list) = function 
    | Skip -> Ok ()

    | Assign(x,e) -> 
        (* the immutable modifier is not checked for the constructor *)
        if f <> "constructor" && is_immutable x (get_state_var_decls vdl) then Error [ImmutabilityError (f,x)]
        else (
          match typecheck_expr f edl vdl fdl e, typecheck_expr f edl vdl fdl (Var x) with
          | Ok(te),Ok(tx) -> if subtype te tx then Ok() else Error [TypeError (f,e,te,tx)]
          | res1,res2 -> typecheck_result_from_expr_result (res1 >>+ res2)
        )

    | Decons(_) -> failwith "TODO: multiple return values"

    | MapW(x,ek,ev) ->  
        (match typecheck_expr f edl vdl fdl (Var x),
               typecheck_expr f edl vdl fdl ek,
               typecheck_expr f edl vdl fdl ev with
          | Ok(tx),Ok(tk),Ok(tv) -> (match tx with
              | MapET(txk,_) when not (subtype tk txk) -> Error [TypeError (f,ek,tk,txk)] 
              | MapET(_,txv) when not (subtype tv txv) -> Error [TypeError (f,ev,tv,txv)] 
              | MapET(_,_) -> Ok()
              | _ -> Error [NotMapError (f,Var x)])
          | res1,res2,res3 -> typecheck_result_from_expr_result (res1 >>+ res2 >>+ res3))

    | Seq(c1,c2) -> 
        typecheck_cmd f edl vdl fdl c1
        >>
        typecheck_cmd f edl vdl fdl c2

    | If(e,c1,c2) -> (match typecheck_expr f edl vdl fdl e with
          | Ok(BoolConstET true)  -> typecheck_cmd f edl vdl fdl c1
          | Ok(BoolConstET false) -> typecheck_cmd f edl vdl fdl c2
          | Ok(BoolET) -> 
              typecheck_cmd f edl vdl fdl c1
              >>
              typecheck_cmd f edl vdl fdl c2
          | Ok(te) -> Error [TypeError (f,e,te,BoolET)]
          | res -> typecheck_result_from_expr_result res)

    | Send(ercv,eamt) -> (match typecheck_expr f edl vdl fdl ercv with
          | Ok(AddrET(true)) -> Ok() (* can only send to payable addresses *)
          | Ok(t_ercv) -> Error [TypeError(f,ercv,t_ercv,AddrET(true))]
          | res -> typecheck_result_from_expr_result res) 
          >>
          (match typecheck_expr f edl vdl fdl eamt with
          | Ok(t_eamt) when subtype t_eamt UintET -> Ok()
          | Ok(t_eamt) -> Error [TypeError(f,eamt,t_eamt,UintET)]
          | res -> typecheck_result_from_expr_result res)

    | Req(e) -> (match typecheck_expr f edl vdl fdl e with
          | Ok(BoolET) -> Ok() 
          | Ok(te) -> Error [TypeError (f,e,te,BoolET)]
          | res -> typecheck_result_from_expr_result res)

    | Block(lvdl,c) ->
        typecheck_local_decls f lvdl
        >>
        let vdl' = push_local_decls vdl lvdl in
        typecheck_cmd f edl vdl' fdl c

    | ExecBlock(_) -> assert(false) (* should not happen at static time *)

    | Decl(_) -> assert(false) (* should not happen after blockify *)

    | ProcCall(e_to, cf, e_value, e_args) -> (
      
      let rec check_args (l1 : local_var_decl list) (l2 : expr list) =
        match (l1,l2) with
        | [],[] -> Ok()
        | (h1::t1, h2::t2) ->
            let ty1 = exprtype_of_decltype (basetype_of_vartype (h1.ty)) in 
            (match typecheck_expr f edl vdl fdl h2 with
            | Ok(ty2) when subtype ty2 ty1 -> check_args t1 t2
            | Ok(ty2) -> Error[TypeError (f, h2, ty2, ty1)]
            | err -> typecheck_result_from_expr_result err)
        | _ -> Error[ParameterMismatch (f, cf)]
      in
      
      let check_value : typecheck_result =
        match typecheck_expr f edl vdl fdl e_value with
        | Ok(t) when subtype t UintET -> Ok()
        | Ok(t) -> Error[TypeError (f, e_value, t, UintET)]
        | err -> typecheck_result_from_expr_result err
      in
      
      let typecheck_proc_internal (_fdl : fun_decl list) (_cf : ide) =
        match (get_al_rt _fdl _cf) with
        | Some(al, _) -> (
          match check_args al e_args with
          | Ok() -> Ok()
          | err -> err
          )
        | None -> Error [FunctionNotFound (f,_cf)]
      in
      
      match typecheck_expr f edl vdl fdl e_to with
      | Ok(AddrET true) -> check_value
        (* Ignores calls to functions in other contracts *)
      | Ok(AddrET false) -> ( (*Non payable*)
        match e_to with
        | This -> (typecheck_result_from_expr_result (check_value_non_payable f e_to e_value)) >> typecheck_proc_internal fdl cf
        | _ -> (typecheck_result_from_expr_result (check_value_non_payable f e_to e_value))
        (* Ignores calls to functions in other contracts *)
      )
      | Ok(t) -> Error[TypeError (f, e_to, t, AddrET false)]
      | _ as err -> typecheck_result_from_expr_result err

    )

    | ExecProcCall(_) -> assert(false) (* should not happen at static time *)

    | Return(el) ->
      let rt =
        match find_fun_in_fdl fdl f with
        | Some(fd) -> (
          match fd with
          | Proc(_, _, _, _, _, rtl) -> (
            match rtl with
            | [] -> Error[MissingReturnType f]
            | [rt] -> Ok(exprtype_of_decltype rt)
            | _::_ -> Error[MultipleReturnTypes f]
          )
          | Constr(_,_,_) -> assert(false)
          )
        | None -> assert(false)
      in
      
      match el with
      | [] -> Error [MissingReturnType f]
      | [h] -> (
        match typecheck_expr f edl vdl fdl h, rt with
        | Ok(t), Ok(rt) when subtype rt t -> Ok()
        | Ok(t), Ok(rt) -> Error[TypeError (f, h, t, rt)] 
        | err1, err2 -> typecheck_result_from_expr_result(err1 >>+ err2)
        )
      | _ -> Error [MultipleReturnTypes f]


let typecheck_fun (edl : enum_decl list) (vdl : var_decl list) (fdl : fun_decl list) = function
  | Constr (al,c,_) ->
      no_dup_local_var_decls "constructor" al
      >>
      typecheck_local_decls "constructor" al
      >> 
      typecheck_cmd "constructor" edl (merge_var_decls vdl al) fdl c
  | Proc (f,al,c,_,_,rtl) ->
      let check_returns =
        match rtl with
        | [] -> Ok()
        | [_] -> if all_paths_return c then Ok() else Error [MissingReturnStatement f]
        | _ -> Error [MultipleReturnTypes f]
      in
      check_returns
      >>
      no_dup_local_var_decls f al
      >> 
      typecheck_local_decls f al
      >>
      typecheck_cmd f edl (merge_var_decls vdl al) fdl c

(* dup_first: finds the first duplicate in a list *)
let rec dup_first (l : 'a list) : 'a option = match l with 
  | [] -> None
  | h::tl -> if List.mem h tl then Some h else dup_first tl

let typecheck_enums (edl : enum_decl list) = 
  match dup_first (List.map (fun (Enum(x,_)) -> x) edl) with
  | Some x -> Error [EnumDupName x] (* there are two enums with the same name *)
  | None -> List.fold_left (fun acc (Enum(x,ol)) -> 
      match dup_first ol with 
      | Some o -> acc >> (Error [EnumDupOption (x,o)])
      | None -> acc
    )
    (Ok ()) 
    edl

(* typecheck_contract : contract -> (unit,string) result 
    Perform several static checks on a given contract. The result is:
    - Ok () if all checks succeed 
    - Error log otherwise, where log explains the reasons of the failed checks     
 *)

let typecheck_contract (Contract(_,edl,vdl,fdl)) : typecheck_result =
  (* no multiply declared enums *)
  typecheck_enums edl 
  >>
  (* no multiply declared state variables *)
  no_dup_var_decls vdl
  >>
  (* no multiply declared functions *)
  no_dup_fun_decls fdl
  >>
  List.fold_left (fun acc fd -> acc >> typecheck_fun edl vdl fdl fd) (Ok ()) fdl  


let string_of_typecheck_result = function
  Ok() -> "Typecheck ok"
| Error log -> List.fold_left 
  (fun acc ex -> acc ^ (if acc="" then "" else "\n") ^ string_of_typecheck_error ex) "" log