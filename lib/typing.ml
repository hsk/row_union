open Ast

let rec flatten_top visited l =
  match l with
  | [] -> []
  | Var v :: xs ->
      if List.mem v.id visited then Var v :: flatten_top visited xs
      else (
        match v.link with
        | Some concrete -> flatten_top (v.id :: visited) (concrete @ xs)
        | None -> Var v :: flatten_top visited xs
      )
  | x :: xs -> x :: flatten_top visited xs

let trail = ref []
let set_link v val_opt =
  let old_link = v.link in
  trail := (fun () -> v.link <- old_link) :: !trail;
  v.link <- val_opt

let with_transaction f =
  let old_trail = !trail in
  trail := [];
  let res = f () in
  if not res then List.iter (fun undo -> undo ()) !trail;
  trail := old_trail;
  res

let structural_eq ~var_eq t1 t2 =
  let rec check_eq hist t1 t2 =
    if List.exists (fun (h1, h2) -> h1 == t1 && h2 == t2) hist then true
    else
      let hist' = (t1, t2) :: hist in
      let t1_flat = flatten_top [] t1 in
      let t2_flat = flatten_top [] t2 in
      if List.length t1_flat <> List.length t2_flat then false
      else
        List.for_all2 (fun e1 e2 ->
          match e1, e2 with
          | Int, Int | Bool, Bool | String, String -> true
          | Arrow (a1, b1), Arrow (a2, b2) -> check_eq hist' a1 a2 && check_eq hist' b1 b2
          | Const (c1, l1), Const (c2, l2) ->
              c1 = c2 && List.length l1 = List.length l2 && List.for_all2 (check_eq hist') l1 l2
          | Var v1, Var v2 -> var_eq v1 v2
          | _ -> false
        ) t1_flat t2_flat
  in
  check_eq [] t1 t2

let same_type t1 t2 =
  let seen = ref [] in
  structural_eq t1 t2 ~var_eq:(fun v1 v2 ->
    v1.id = v2.id
    || List.mem (v1.id, v2.id) !seen
    || (v1.link <> None && v2.link <> None
        && (seen := (v1.id, v2.id) :: !seen; true)))

let alpha_equal t1 t2 =
  let pairs = ref [] in
  structural_eq t1 t2 ~var_eq:(fun v1 v2 ->
    v1.id = v2.id
    || (match List.assoc_opt v1.id !pairs with
        | Some id2 -> id2 = v2.id
        | None ->
            not (List.exists (fun (_, id2) -> id2 = v2.id) !pairs)
            && (pairs := (v1.id, v2.id) :: !pairs; true)))

let rec subtype hist t1 t2 =
  if List.exists (fun (x, y) -> same_type x t1 && same_type y t2) hist then true
  else sub_core ((t1, t2) :: hist) (flatten_top [] t1) (flatten_top [] t2)
and sub_core hist t1 t2 =
  if same_type t1 t2 then true
  else match t1, t2 with
  | [Var v], _ when v.link = None -> set_link v (Some t2); true
  | _, [Var v] when v.link = None -> set_link v (Some t1); true
  | _ -> List.for_all (fun x -> try_match_any hist x t2) t1   (* sub_list を消去して1行化 *)
and try_match_any hist x ys =
  List.exists (fun y -> with_transaction (fun () -> elem_sub hist x y)) ys
and elem_sub hist e1 e2 =
  match e1, e2 with
  | Arrow (a1, b1), Arrow (a2, b2) -> subtype hist a2 a1 && subtype hist b1 b2
  | Const (c1, args1), Const (c2, args2) when c1 = c2 ->
      List.length args1 = List.length args2 && List.for_all2 (subtype hist) args1 args2
  | Var v1, _ when v1.link = None -> set_link v1 (Some [e2]); true
  | _, Var v2 when v2.link = None -> set_link v2 (Some [e1]); true
  | _ -> same_type [e1] [e2]

let rec pat p ty_list =
  let ty_list = flatten_top [] ty_list in
  match p, ty_list with
  | PInt _, [Int] | PBool _, [Bool] -> []
  | PVar x, t -> [(x, t)]
  | PAnn (x, t_ann), t ->
      if subtype [] t t_ann && subtype [] t_ann t then [(x, t_ann)]
      else failwith "Pattern annotation mismatch"
  | PConst (c, args), [Const (c_ty, ts)] when c = c_ty ->
      List.flatten (List.map2 pat args ts)
  | _, [Var v] when v.link = None ->
      (match p with
       | PInt _ -> set_link v (Some [Int]); []
       | PBool _ -> set_link v (Some [Bool]); []
       | PVar x -> [(x, [Var v])]
       | PAnn (x, t_ann) -> set_link v (Some t_ann); [(x, t_ann)]
       | PConst (c, args) ->
           let ts = List.map (fun _ -> [Var (new_var ())]) args in (* 1行に短縮 *)
           set_link v (Some [Const (c, ts)]);
           List.flatten (List.map2 pat args ts)
       | PDefault -> failwith "Unexpected default in pat")
  | _ -> failwith "Pattern matching type mismatch"

let rec check gamma expr =
  match expr with
  | VarExpr x -> (try List.assoc x gamma with Not_found -> failwith ("Variable not found: " ^ x))
  | IntExpr _ -> [Int]
  | BoolExpr _ -> [Bool]
  | Fix (x, e) ->
      let t = [Var (new_var ())] in
      if subtype [] (check ((x, t) :: gamma) e) t then t else failwith "Fix type mismatch"
  | Let (x, e1, e2) ->
      let t1 = check gamma e1 in
      check ((x, t1) :: gamma) e2
  | Lam (x, e) ->
      let t1 = [Var (new_var ())] in
      [Arrow (t1, check ((x, t1) :: gamma) e)]
  | ConstApp (c, es) -> [Const (c, List.map (check gamma) es)]
  | App (e1, e2) ->
      let t1, t2 = match flatten_top [] (check gamma e1) with
       | [Arrow (t1, t2)] -> t1, t2
       | [Var v] when v.link = None ->
           let t1, t2 = [Var (new_var ())], [Var (new_var ())] in
           set_link v (Some [Arrow (t1, t2)]); (t1, t2)
       | _ -> failwith "Expected arrow type"
      in
      if subtype [] (check gamma e2) t1 then t2 else failwith "App argument type mismatch"
  | Add (e1, e2) ->
      if subtype [] (check gamma e1) [Int] && subtype [] (check gamma e2) [Int] then [Int]
      else failwith "Type error in addition"
  | Match (e, cs) ->
      let t = [Var (new_var ())] in
      let t2 = check gamma e in
      let t3 = cases gamma cs t2 t in
      if List.exists (fun (p, _) -> p = PDefault) cs then (
        match t2 with
        | [Var v] when v.link = None -> if t3 <> [] then set_link v (Some t3); t
        | _ -> List.iter (fun x -> ignore (try_match_any [] x t3)) t2; t
      ) else (
        if subtype [] t2 t3 then t else failwith "Match type mismatch"
      )
and cases gamma branches t_match t2 =
  match branches with
  | [] -> []
  | (PDefault, e1) :: _ ->
      if subtype [] (check (("default", t_match) :: gamma) e1) t2 then []
      else failwith "Default branch type mismatch"
  | (pat_ast, e1) :: cs ->
      let head = Var (new_var ()) in
      if subtype [] (check (pat pat_ast [head] @ gamma) e1) t2 then
        head :: cases gamma cs t_match t2
      else failwith "Branch type mismatch"
