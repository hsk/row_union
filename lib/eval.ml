open Ast

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env
  | VConst of string * value list
  | VCell of value ref
and env = (string * value) list

let lookup x env =
  let rec unwrap = function
    | VCell r -> unwrap !r
    | v -> v
  in
  try unwrap (List.assoc x env)
  with Not_found -> failwith ("Runtime Error: Variable not found: " ^ x)

let rec check_val_type (v : value) (t : t) : bool =
  List.exists (fun typ -> check_val_single_type v typ) t
and check_val_single_type (v : value) (typ : typ) : bool =
  match v, typ with
  | VInt _, Int -> true
  | VBool _, Bool -> true
  | VConst (c1, vals), Const (c2, args) when c1 = c2 ->
      (try List.for_all2 check_val_type vals args
       with Invalid_argument _ -> false)
  | _ -> false

let rec match_pattern (pat : pattern) (val_ : value) (env : env) : env option =
  match pat with
  | PInt i ->
      (match val_ with VInt v when v = i -> Some env | _ -> None)
  | PBool b ->
      (match val_ with VBool v when v = b -> Some env | _ -> None)
  | PVar x ->
      Some ((x, val_) :: env)
  | PAnn (x, t) ->
      if check_val_type val_ t then Some ((x, val_) :: env) else None
  | PConst (c, pats) ->
      (match val_ with
      | VConst (c_val, vals) when c = c_val -> match_nested_patterns pats vals env
      | _ -> None)
  | PDefault ->
      Some (("default", val_) :: env)
and match_nested_patterns pats vals env =
  match pats, vals with
  | [], [] -> Some env
  | p :: ps, v :: vs ->
      (match match_pattern p v env with
      | Some env1 -> match_nested_patterns ps vs env1
      | None -> None)
  | _ -> None

let rec eval (e : expr) (env : env) : value =
  match e with
  | IntExpr i -> VInt i
  | BoolExpr b -> VBool b
  | VarExpr x -> lookup x env
  | Lam (x, body) -> VClosure (x, body, env)
  | Fix (x, body) ->
      let cell = ref (VInt 0) in
      let extended_env = (x, VCell cell) :: env in
      let v = eval body extended_env in
      cell := v;
      v
  | ConstApp (c, es) ->
      VConst (c, List.map (fun expr -> eval expr env) es)
  | App (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
      | VClosure (x, body, c_env) -> eval body ((x, v2) :: c_env)
      | _ -> failwith "Runtime Error: Application of a non-function value")
  | Add (e1, e2) ->
      (match eval e1 env, eval e2 env with
      | VInt n1, VInt n2 -> VInt (n1 + n2)
      | _ -> failwith "Runtime Error: Integer expected for addition")
  | Let (x, e1, e2) ->
      let v1 = eval e1 env in
      eval e2 ((x, v1) :: env)
  | Match (e, branches) ->
      let v = eval e env in
      eval_cases branches v env
and eval_cases branches val_ env =
  match branches with
  | [] -> failwith "Runtime Error: Pattern match failure"
  | (pat, body) :: rest ->
      match match_pattern pat val_ env with
      | Some extended_env -> eval body extended_env
      | None -> eval_cases rest val_ env

let rec show_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VClosure _ -> "<fun>"
  | VConst (c, []) -> c
  | VConst (c, args) -> c ^ "(" ^ String.concat ", " (List.map show_val args) ^ ")"
  | VCell r -> show_val !r

let eval_prog ast =
  let rec loop gamma env = function
    | [] -> (gamma,env)
    | decl :: rest ->
        match decl with
        | ExprDecl e ->
            let t = Typing.check gamma e in
            let v = eval e env in
            Printf.printf "- : %s = %s\n" (show_t t) (show_val v);
            loop gamma env rest
        | LetDecl (x, e) ->
            let t = Typing.check gamma e in
            let v = eval e env in
            Printf.printf "val %s : %s = %s\n" x (show_t t) (show_val v);
            let gamma' = (x, t) :: gamma in
            let env' = (x, v) :: env in
            loop gamma' env' rest
  in
  loop [] [] ast
