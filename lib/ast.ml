type typ =
  | Int
  | Bool
  | String
  | Arrow of t * t
  | Const of string * t list
  | Var of tvar
and tvar = {
  id : int;
  mutable link : t option;
}
and t = typ list

let tvar_counter = ref 0
let new_tvar () =
  incr tvar_counter;
  { id = !tvar_counter; link = None }

let tvar_map: (string * tvar) list ref = ref []
let reset_tvars () =
  tvar_map := []
let get_tvar name =
  try List.assoc name !tvar_map
  with Not_found ->
    let v = new_tvar () in
    tvar_map := (name, v) :: !tvar_map;
    v

let tvar_names = ref []
let next_name_idx = ref 0
let looped_ids = ref []
let defined_ids = ref []

let get_var_name id =
  try List.assoc id !tvar_names
  with Not_found ->
    let name =
      let idx = !next_name_idx in
      incr next_name_idx;
      if idx < 26 then
        Printf.sprintf "'%c" (char_of_int (int_of_char 'a' + idx))
      else
        Printf.sprintf "'a%d" (idx - 25)
    in
    tvar_names := (id, name) :: !tvar_names;
    name

let rec show_t visited l =
  if visited = [] then (
    tvar_names := []; 
    next_name_idx := 0;
    looped_ids := [];
    defined_ids := []
  );
  match l with
  | [] -> "Bot"
  | [t] -> show_typ visited false t
  | _ -> String.concat " | " (List.map (show_typ visited true) l)
and show_typ visited in_union = function
  | Int -> "Int" 
  | Bool -> "Bool" 
  | String -> "String"
  | Arrow (a, b) -> 
      let arrow_str = show_t visited a ^ " -> " ^ show_t visited b in
      if in_union then "(" ^ arrow_str ^ ")" else arrow_str
  | Const (c, []) -> c
  | Const (c, args) -> 
      c ^ "(" ^ String.concat ", " (List.map (show_t visited) args) ^ ")"
  | Var v ->
      let name = get_var_name v.id in
      if List.mem v.id visited then begin
        if not (List.mem v.id !looped_ids) then looped_ids := v.id :: !looped_ids;
        name
      end else if List.mem v.id !defined_ids then
        name
      else match v.link with
        | None -> name
        | Some concrete ->
            let res = show_t (v.id :: visited) concrete in
            if List.mem v.id !looped_ids then begin
              defined_ids := v.id :: !defined_ids;
              let needs_parens = 
                match concrete with 
                | [] | _ :: _ :: _ -> true 
                | [Arrow _] -> true 
                | _ -> false 
              in
              let base = if needs_parens then "(" ^ res ^ ")" else res in
              let as_str = base ^ " as " ^ name in
              if in_union then "(" ^ as_str ^ ")" else as_str
            end else
              res

let show_t = show_t []
let show_typ = show_typ []

type expr =
  | VarExpr of string
  | IntExpr of int
  | BoolExpr of bool
  | Fix of string * expr            (* let rec f x1 x2 ... xn = e *)
  | Lam of string * expr            (* fun x -> e *)
  | Let of string * expr * expr     (* let x = e1 in e2 *)
  | ConstApp of string * expr list  (* C(e1,e2,...,en) *)
  | App of expr * expr              (* e1 e2 *)
  | Add of expr * expr              (* e1 e2 *)
  | Match of expr * branch list     (* match e with | p1 -> e2 |...| pn -> en *)
and branch = pattern * expr
and pattern =
  | PInt of int
  | PBool of bool
  | PVar of string
  | PAnn of string * t                    (* x:t *)
  | PConst of string * pattern list       (* C(p1,p2,...,pn) *)
  | PDefault                              (* _ *)

let var_counter = ref 0
let new_var () =
  incr var_counter;
  { id = !var_counter; link = None }

type decl =
  | LetDecl of string * expr        (* let x = e or let rec f = e *)
  | ExprDecl of expr                (* expression *)
