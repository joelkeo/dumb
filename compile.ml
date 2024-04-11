open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors
open Set
open Graph

module StringSet = Set.Make(String)


type 'a name_envt = (string * 'a) list
type 'a tag_envt  = (tag * 'a) list



let print_env env how =
  debug_printf "Env is\n";
  List.iter (fun (id, bind) -> debug_printf "  %s -> %s\n" id (how bind)) env;;


let const_true       = HexConst(0xFFFFFFFFFFFFFFFFL)
let const_false      = HexConst(0x7FFFFFFFFFFFFFFFL)
let bool_mask        = HexConst(0x8000000000000000L)
let bool_tag         = 0x0000000000000007L
let bool_tag_mask    = 0x0000000000000007L
let num_tag          = 0x0000000000000000L
let num_tag_mask     = 0x0000000000000001L
let closure_tag      = 0x0000000000000005L
let closure_tag_mask = 0x0000000000000007L
let tuple_tag        = 0x0000000000000001L
let tuple_tag_mask   = 0x0000000000000007L
let lambda_tag       = 0x0000000000000005L
let const_nil        = HexConst(tuple_tag)

let err_COMP_NOT_NUM     = 1L
let err_ARITH_NOT_NUM    = 2L
let err_LOGIC_NOT_BOOL   = 3L
let err_IF_NOT_BOOL      = 4L
let err_OVERFLOW         = 5L
let err_GET_NOT_TUPLE    = 6L
let err_GET_LOW_INDEX    = 7L
let err_GET_HIGH_INDEX   = 8L
let err_NIL_DEREF        = 9L
let err_OUT_OF_MEMORY    = 10L
let err_SET_NOT_TUPLE    = 11L
let err_SET_LOW_INDEX    = 12L
let err_SET_HIGH_INDEX   = 13L
let err_CALL_NOT_CLOSURE = 14L
let err_CALL_ARITY_ERR   = 15L

let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos);;

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]
let heap_reg = R15
let scratch_reg = R11

(* you can add any functions or data defined by the runtime here for future use *)
(* how ??? *)
let initial_val_env = [];;
let prim_bindings = [];;
let native_fun_bindings = [];;
let initial_fun_env = prim_bindings @ native_fun_bindings;;
let pw = (Sized(QWORD_PTR, Const(0L)));;

(* You may find some of these helpers useful *)

let cmap f l = 
  fst(List.fold_left((fun (acc_list, acc_i) current -> (acc_list @ [(f current acc_i)], acc_i + 1)))
    ([], 0)
    l);;

(* in this case, the function should produce a list *)
let cmap_star f l = 
  fst(List.fold_left((fun (acc_list, acc_i) current -> (acc_list @ (f current acc_i), acc_i + 1)))
    ([], 0)
    l);;  

let tag_to_lambda_name tag = 
  sprintf "lambda_%d" tag;;

let round_even n = 
  if n == 0 then 0 else
    (if (n mod 2) == 1 then n + 1 else n);;

let isOdd n = 
  not (n == 0 || n mod 2 == 0);;

let get_clambda_values lam = 
  match lam with
    | CLambda(name, args, body, tag) -> (name, args, body, tag)
    | _ -> failwith "tried to get a clambda, but its not one"


type error_type = 
| CMP_NOT_NUM 
| ARITH_NOT_NUM
| LOGIC_NOT_BOOL
| IF_NOT_BOOL
| OVERFLOW
| GET_NOT_TUPLE
| GET_LOW_IDX
| GET_HIGH_IDX
| GET_NOT_NUM
| NIL_DEREF
| OOM
| SET_NOT_TUPLE
| SET_LOW__IDX
| SET_NOT_NUM
| SET_HIGH__IDX
| CALL_NOT_CLOSURE
| CALL_ARITY

let error_type_to_string (err : error_type) : string =
match err with
| CMP_NOT_NUM -> "?err_cmp_not_num"
| ARITH_NOT_NUM -> "?err_arith_not_num"
| LOGIC_NOT_BOOL -> "?err_logic_not_bool"
| IF_NOT_BOOL -> "?err_if_not_bool"
| OVERFLOW -> "?err_overflow"
| GET_NOT_TUPLE -> "?err_get_not_tuple"
| GET_LOW_IDX -> "?err_get_low_index"
| GET_HIGH_IDX -> "?err_get_high_index"
| GET_NOT_NUM -> "?err_get_not_num"
| NIL_DEREF -> "?err_nil_deref"
| OOM -> "?err_out_of_memory"
| SET_NOT_TUPLE -> "?err_set_not_tuple"
| SET_LOW__IDX -> "?err_set_low_index"
| SET_NOT_NUM -> "?err_set_not_num"
| SET_HIGH__IDX -> "?err_set_high_index"
| CALL_NOT_CLOSURE -> "?err_call_not_closure"
| CALL_ARITY -> "?err_call_arity_err"
;;

let error_type_to_code (err : error_type) : int64 =
Int64.of_int 
 (match err with
 | CMP_NOT_NUM -> 1
 | ARITH_NOT_NUM -> 2
 | LOGIC_NOT_BOOL -> 3
 | IF_NOT_BOOL -> 4
 | OVERFLOW -> 5
 | GET_NOT_TUPLE -> 6
 | GET_LOW_IDX -> 7
 | GET_HIGH_IDX -> 8
 | GET_NOT_NUM -> 9
 | NIL_DEREF -> 10
 | OOM -> 11
 | SET_NOT_TUPLE -> 12
 | SET_LOW__IDX -> 13
 | SET_NOT_NUM -> 14
 | SET_HIGH__IDX -> 15
 | CALL_NOT_CLOSURE -> 16
 | CALL_ARITY -> 17)
;;


let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y,v)::rest ->
     if y = x then v else find rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq(e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ALetRec(binds, body, _) ->
       (List.length binds) + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | CLPrim2(_, _, e, _) -> helpA e
    | _ -> 0
  in helpA e

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))


let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs
;;

let rec find_opt (env : 'a name_envt) (elt: string) : 'a option =
  match env with
  | [] -> None
  | (x, v) :: rst -> if x = elt then Some(v) else find_opt rst elt
;;
                             
(* Prepends a list-like env onto an name_envt *)
let merge_envs list_env1 list_env2 =
  list_env1 @ list_env2
;;
(* Combines two name_envts into one, preferring the first one *)
let prepend env1 env2 =
  let rec help env1 env2 =
    match env1 with
    | [] -> env2
    | ((k, _) as fst)::rst ->
      let rst_prepend = help rst env2 in
      if List.mem_assoc k env2 then rst_prepend else fst::rst_prepend
  in
  help env1 env2
;;

let env_keys e = List.map fst e;;

(* Scope_info stores the location where something was defined,
   and if it was a function declaration, then its type arity and argument arity *)
type scope_info = (sourcespan * int option * int option)
let is_well_formed (p : sourcespan program) : (sourcespan program) fallible =
  let rec wf_E e (env : scope_info name_envt) =
    (* debug_printf "In wf_E: %s\n" (ExtString.String.join ", " (env_keys env)); *)
    match e with
    | ESeq(e1, e2, _) -> wf_E e1 env @ wf_E e2 env
    | ETuple(es, _) -> List.concat (List.map (fun e -> wf_E e env) es)
    | EGetItem(e, idx, pos) ->
       wf_E e env @ wf_E idx env
    | ESetItem(e, idx, newval, pos) ->
       wf_E e env @ wf_E idx env @ wf_E newval env
    | ENil _ -> []
    | EBool _ -> []
    | ENumber(n, loc) ->
       if n > (Int64.div Int64.max_int 2L) || n < (Int64.div Int64.min_int 2L) then
         [Overflow(n, loc)]
       else
         []
    | EId (x, loc) -> if (find_one (List.map fst env) x) then [] else [UnboundId(x, loc)]
    | EPrim1(_, e, _) -> wf_E e env
    | EPrim2(_, l, r, _) -> wf_E l env @ wf_E r env
    | EIf(c, t, f, _) -> wf_E c env @ wf_E t env @ wf_E f env
    | ELet(bindings, body, _) ->
       let rec find_locs x (binds : 'a bind list) : 'a list =
         match binds with
         | [] -> []
         | BBlank _::rest -> find_locs x rest
         | BName(y, _, loc)::rest ->
            if x = y then loc :: find_locs x rest
            else  find_locs x rest
         | BTuple(binds, _)::rest -> find_locs x binds @ find_locs x rest in
       let rec find_dupes (binds : 'a bind list) : exn list =
         match binds with
         | [] -> []
         | (BBlank _::rest) -> find_dupes rest
         | (BName(x, _, def)::rest) -> (List.map (fun use -> DuplicateId(x, use, def)) (find_locs x rest)) @ (find_dupes rest)
         | (BTuple(binds, _)::rest) -> find_dupes (binds @ rest) in
       let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) bindings) in
       let rec process_binds (rem_binds : 'a bind list) (env : scope_info name_envt) =
         match rem_binds with
         | [] -> (env, [])
         | BBlank _::rest -> process_binds rest env
         | BTuple(binds, _)::rest -> process_binds (binds @ rest) env
         | BName(x, allow_shadow, xloc)::rest ->
            let shadow =
              if allow_shadow then []
              else match find_opt env x with
                   | None -> []
                   | Some (existing, _, _) -> [ShadowId(x, xloc, existing)] in
            let new_env = (x, (xloc, None, None))::env in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs)) in
       let rec process_bindings bindings (env : scope_info name_envt) =
         match bindings with
         | [] -> (env, [])
         | (b, e, loc)::rest ->
            let errs_e = wf_E e env in
            let (env', errs) = process_binds [b] env in
            let (env'', errs') = process_bindings rest env' in
            (env'', errs @ errs_e @ errs') in
       let (env2, errs) = process_bindings bindings env in
       dupeIds @ errs @ wf_E body env2
    | EApp(func, args, native, loc) ->
       let rec_errors = List.concat (List.map (fun e -> wf_E e env) (func :: args)) in
       (match func with
        | EId(funname, _) -> 
           (match (find_opt env funname) with
            | Some(_, _, Some arg_arity) ->
               let actual = List.length args in
               if actual != arg_arity then [Arity(arg_arity, actual, loc)] else []
            | _ -> [])
        | _ -> [])
       @ rec_errors
    | ELetRec(binds, body, _) ->
       let nonfuns = List.find_all (fun b -> match b with | (BName _, ELambda _, _) -> false | _ -> true) binds in
       let nonfun_errs = List.map (fun (b, _, where) -> LetRecNonFunction(b, where)) nonfuns in

     
       let rec find_locs x (binds : 'a bind list) : 'a list =
         match binds with
         | [] -> []
         | BBlank _::rest -> find_locs x rest
         | BName(y, _, loc)::rest ->
            if x = y then loc :: find_locs x rest
            else  find_locs x rest
         | BTuple(binds, _)::rest -> find_locs x binds @ find_locs x rest in
       let rec find_dupes (binds : 'a bind list) : exn list =
         match binds with
         | [] -> []
         | (BBlank _::rest) -> find_dupes rest
         | (BName(x, _, def)::rest) -> List.map (fun use -> DuplicateId(x, use, def)) (find_locs x rest)
         | (BTuple(binds, _)::rest) -> find_dupes (binds @ rest) in
       let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) binds) in
       let rec process_binds (rem_binds : sourcespan bind list) (env : scope_info name_envt) =
         match rem_binds with
         | [] -> (env, [])
         | BBlank _::rest -> process_binds rest env
         | BTuple(binds, _)::rest -> process_binds (binds @ rest) env
         | BName(x, allow_shadow, xloc)::rest ->
            let shadow =
              if allow_shadow then []
              else match (find_opt env x) with
                   | None -> []
                   | Some (existing, _, _) -> if xloc = existing then [] else [ShadowId(x, xloc, existing)] in
            let new_env = (x, (xloc, None, None))::env in
            let (newer_env, errs) = process_binds rest new_env in
            (newer_env, (shadow @ errs)) in

       let (env, bind_errs) = process_binds (List.map (fun (b, _, _) -> b) binds) env in
       
       let rec process_bindings bindings env =
         match bindings with
         | [] -> (env, [])
         | (b, e, loc)::rest ->
            let (env, errs) = process_binds [b] env in
            let errs_e = wf_E e env in
            let (env', errs') = process_bindings rest env in
            (env', errs @ errs_e @ errs') in
       let (new_env, binding_errs) = process_bindings binds env in

       let rhs_problems = List.map (fun (_, rhs, _) -> wf_E rhs new_env) binds in
       let body_problems = wf_E body new_env in
       nonfun_errs @ dupeIds @ bind_errs @ binding_errs @ (List.flatten rhs_problems) @ body_problems
    | ELambda(binds, body, _) ->
       let rec dupe x args =
         match args with
         | [] -> None
         | BName(y, _, loc)::_ when x = y -> Some loc
         | BTuple(binds, _)::rest -> dupe x (binds @ rest)
         | _::rest -> dupe x rest in
       let rec process_args rem_args =
         match rem_args with
         | [] -> []
         | BBlank _::rest -> process_args rest
         | BName(x, _, loc)::rest ->
            (match dupe x rest with
             | None -> []
             | Some where -> [DuplicateId(x, where, loc)]) @ process_args rest
         | BTuple(binds, loc)::rest ->
            process_args (binds @ rest)
       in
       let rec flatten_bind (bind : sourcespan bind) : (string * scope_info) list =
         match bind with
         | BBlank _ -> []
         | BName(x, _, xloc) -> [(x, (xloc, None, None))]
         | BTuple(args, _) -> List.concat (List.map flatten_bind args) in
       (process_args binds) @ wf_E body (merge_envs (List.concat (List.map flatten_bind binds)) env)
  and wf_D d (env : scope_info name_envt) (tyenv : StringSet.t) =
    match d with
    | DFun(_, args, body, _) ->
       let rec dupe x args =
         match args with
         | [] -> None
         | BName(y, _, loc)::_ when x = y -> Some loc
         | BTuple(binds, _)::rest -> dupe x (binds @ rest)
         | _::rest -> dupe x rest in
       let rec process_args rem_args =
         match rem_args with
         | [] -> []
         | BBlank _::rest -> process_args rest
         | BName(x, _, loc)::rest ->
            (match dupe x rest with
             | None -> []
             | Some where -> [DuplicateId(x, where, loc)]) @ process_args rest
         | BTuple(binds, loc)::rest ->
            process_args (binds @ rest)
       in
       let rec arg_env args (env : scope_info name_envt) =
         match args with
         | [] -> env
         | BBlank _ :: rest -> arg_env rest env
         | BName(name, _, loc)::rest -> (name, (loc, None, None))::(arg_env rest env)
         | BTuple(binds, _)::rest -> arg_env (binds @ rest) env in
       (process_args args) @ (wf_E body (arg_env args env))
  and wf_G (g : sourcespan decl list) (env : scope_info name_envt) (tyenv : StringSet.t) =
    let add_funbind (env : scope_info name_envt) d =
      match d with
      | DFun(name, args, _, loc) ->
         (name, (loc, Some (List.length args), Some (List.length args)))::env in
    let env = List.fold_left add_funbind env g in
    let errs = List.concat (List.map (fun d -> wf_D d env tyenv) g) in
    (errs, env)
  in
  match p with
  | Program(decls, body, _) ->
     let initial_env = initial_val_env in
     let initial_env = List.fold_left
                          (fun env (name, (_, arg_count)) -> (name, (dummy_span, Some arg_count, Some arg_count))::env)
     initial_fun_env
     initial_env in
     let rec find name (decls : 'a decl list) =
       match decls with
       | [] -> None
       | DFun(n, args, _, loc)::rest when n = name -> Some(loc)
       | _::rest -> find name rest in
     let rec dupe_funbinds decls =
       match decls with
       | [] -> []
       | DFun(name, args, _, loc)::rest ->
          (match find name rest with
          | None -> []
          | Some where -> [DuplicateFun(name, where, loc)]) @ dupe_funbinds rest in
     let all_decls = List.flatten decls in
     let initial_tyenv = StringSet.of_list ["Int"; "Bool"] in
     let help_G (env, exns) g =
       let (g_exns, funbinds) = wf_G g env initial_tyenv in
       (List.fold_left (fun xs x -> x::xs) env funbinds, exns @ g_exns) in
     let (env, exns) = List.fold_left help_G (initial_env, dupe_funbinds all_decls) decls in
     (* debug_printf "In wf_P: %s\n" (ExtString.String.join ", " (env_keys env)); *)
     let exns = exns @ (wf_E body env)
     in match exns with
        | [] -> Ok p
        | _ -> Error exns
;;

(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; DESUGARING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)

let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    (fun name ->
      next := !next + 1;
      sprintf "%s_%d" name (!next)) in
  let rec helpP (p : sourcespan program) =
    match p with
    | Program(decls, body, tag) ->
       (* This particular desugaring will convert declgroups into ELetRecs *)
       let merge_sourcespans ((s1, _) : sourcespan) ((_, s2) : sourcespan) : sourcespan = (s1, s2) in
       let wrap_G g body =
         match g with
         | [] -> body
         | f :: r ->
            let span = List.fold_left merge_sourcespans (get_tag_D f) (List.map get_tag_D r) in
            ELetRec(helpG g, body, span) in
       Program([], List.fold_right wrap_G decls (helpE body), tag)
  and helpG g =
    List.map helpD g
  and helpD d =
    match d with
    | DFun(name, args, body, tag) ->
       let helpArg a =
         match a with
         | BTuple(_, tag) ->
            let name = gensym "argtup" in
            let newbind = BName(name, false, tag) in
            (newbind, [(a, EId(name, tag), tag)])
         | _ -> (a, []) in
       let (newargs, argbinds) = List.split (List.map helpArg args) in
       let newbody = ELet(List.flatten argbinds, body, tag) in
       (BName(name, false, tag), ELambda(newargs, helpE newbody, tag), tag)
  and helpBE bind =
    let (b, e, btag) = bind in
    let e = helpE e in
    match b with
    | BTuple(binds, ttag) ->
       (match e with
        | EId _ ->
           expandTuple binds ttag e
        | _ ->
           let newname = gensym "tup" in
           (BName(newname, false, ttag), e, btag) :: expandTuple binds ttag (EId(newname, ttag)))
    | _ -> [(b, e, btag)]
  and expandTuple binds tag source : sourcespan binding list =
    let tupleBind i b =
      match b with
      | BBlank btag -> []
      | BName(_, _, btag) ->
        [(b, EGetItem(source, ENumber(Int64.of_int(i), dummy_span), tag), btag)]
      | BTuple(binds, tag) ->
          let newname = gensym "tup" in
          let newexpr = EId(newname, tag) in
          (BName(newname, false, tag), EGetItem(source, ENumber(Int64.of_int(i), dummy_span), tag), tag) :: expandTuple binds tag newexpr
    in
    let size_check = EPrim2(CheckSize, source, ENumber(Int64.of_int(List.length binds), dummy_span), dummy_span) in
    let size_check_bind = (BBlank(dummy_span), size_check, dummy_span) in
    size_check_bind::(List.flatten (List.mapi tupleBind binds))
  and helpE e =
    match e with
    | ESeq(e1, e2, tag) -> ELet([(BBlank(tag), helpE e1, tag)], helpE e2, tag)
    | ETuple(exprs, tag) -> ETuple(List.map helpE exprs, tag)
    | EGetItem(e, idx, tag) -> EGetItem(helpE e, helpE idx, tag)
    | ESetItem(e, idx, newval, tag) -> ESetItem(helpE e, helpE idx, helpE newval, tag)
    | EId(x, tag) -> EId(x, tag)
    | ENumber(n, tag) -> ENumber(n, tag)
    | EBool(b, tag) -> EBool(b, tag)
    | ENil(t, tag) -> ENil(t, tag)
    | EPrim1(op, e, tag) ->
       EPrim1(op, helpE e, tag)
    | EPrim2(op, e1, e2, tag) ->
       EPrim2(op, helpE e1, helpE e2, tag)
    | ELet(binds, body, tag) ->
       let newbinds = (List.map helpBE binds) in
       List.fold_right (fun binds body -> ELet(binds, body, tag)) newbinds (helpE body)
    | ELetRec(bindexps, body, tag) ->
       (* ASSUMES well-formed letrec, so only BName bindings *)
       let newbinds = (List.map (fun (bind, e, tag) -> (bind, helpE e, tag)) bindexps) in
       ELetRec(newbinds, helpE body, tag)
    | EIf(cond, thn, els, tag) ->
       EIf(helpE cond, helpE thn, helpE els, tag)
    | EApp(name, args, native, tag) ->
       EApp(helpE name, List.map helpE args, native, tag)
    | ELambda(binds, body, tag) ->
       let expandBind bind =
         match bind with
         | BTuple(_, btag) ->
            let newparam = gensym "tuparg" in
            (BName(newparam, false, btag), helpBE (bind, EId(newparam, btag), btag))
         | _ -> (bind, []) in
       let (params, newbinds) = List.split (List.map expandBind binds) in
       let newbody = List.fold_right (fun binds body -> ELet(binds, body, tag)) newbinds (helpE body) in
       ELambda(params, newbody, tag)

  in helpP p
;;

(* ASSUMES desugaring is complete *)
let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program(decls, body, tag) ->
       Program(List.map (fun group -> List.map (helpD env) group) decls, helpE env body, tag)
  and helpD env decl =
    match decl with
    | DFun(name, args, body, tag) ->
       let (newArgs, env') = helpBS env args in
       DFun(name, newArgs, helpE env' body, tag)
  and helpB env b =
    match b with
    | BBlank tag -> (b, env)
    | BName(name, allow_shadow, tag) ->
       let name' = sprintf "%s_%d" name tag in
       (BName(name', allow_shadow, tag), (name, name') :: env)
    | BTuple(binds, tag) ->
       let (binds', env') = helpBS env binds in
       (BTuple(binds', tag), env')
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b::bs ->
       let (b', env') = helpB env b in
       let (bs', env'') = helpBS env' bs in
       (b'::bs', env'')
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a)::bindings ->
       let (b', env') = helpB env b in
       let e' = helpE env e in
       let (bindings', env'') = helpBG env' bindings in
       ((b', e', a)::bindings', env'')
  and helpE env e =
    match e with
    | ESeq(e1, e2, tag) -> ESeq(helpE env e1, helpE env e2, tag)
    | ETuple(es, tag) -> ETuple(List.map (helpE env) es, tag)
    | EGetItem(e, idx, tag) -> EGetItem(helpE env e, helpE env idx, tag)
    | ESetItem(e, idx, newval, tag) -> ESetItem(helpE env e, helpE env idx, helpE env newval, tag)
    | EPrim1(op, arg, tag) -> EPrim1(op, helpE env arg, tag)
    | EPrim2(op, left, right, tag) -> EPrim2(op, helpE env left, helpE env right, tag)
    | EIf(c, t, f, tag) -> EIf(helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId(name, tag) ->
       (try
         EId(find env name, tag)
       with InternalCompilerError _ -> e)
    | EApp(func, args, native, tag) ->
       let func = helpE env func in
       let call_type =
         (* TODO: If you want, try to determine whether func is a known function name, and if so,
            whether it's a Snake function or a Native function *)
         Snake in
       EApp(func, List.map (helpE env) args, call_type, tag)
    | ELet(binds, body, tag) ->
       let (binds', env') = helpBG env binds in
       let body' = helpE env' body in
       ELet(binds', body', tag)
    | ELetRec(bindings, body, tag) ->
       let (revbinds, env) = List.fold_left (fun (revbinds, env) (b, e, t) ->
                                 let (b, env) = helpB env b in ((b, e, t)::revbinds, env)) ([], env) bindings in
       let bindings' = List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag)::bindings) [] revbinds in
       let body' = helpE env body in
       ELetRec(bindings', body', tag)
    | ELambda(binds, body, tag) ->
       let (binds', env') = helpBS env binds in
       let body' = helpE env' body in
       ELambda(binds', body', tag)
  in (rename [] p)
;;


(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;; ANFING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)


type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program([], body, _) -> AProgram(helpA body, ())
    | Program _ -> raise (InternalCompilerError "decls should have been desugared away")
  and helpC (e : tag expr) : (unit cexpr * unit anf_bind list) = 
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(And, arg1, arg2, _) ->
      let (arg1_imm, arg1_setup) = helpI arg1 in
      let arg2_a = helpA arg2 in
      (CLPrim2(And, arg1_imm, arg2_a, ()), arg1_setup)
  | EPrim2(Or, arg1, arg2, _) ->
      let (arg1_imm, arg1_setup) = helpI arg1 in
      let arg2_a = helpA arg2 in
      (CLPrim2(Or, arg1_imm, arg2_a, ()), arg1_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (CIf(cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet([], body, _) -> helpC body
    | ELet((BBlank _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BSeq exp_ans] @ body_setup)
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELetRec(binds, body, _) ->
       let processBind (bind, rhs, _) =
         match bind with
         | BName(name, _, _) -> (name, helpC rhs)
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a let-rec: %s"
                                             (string_of_bind bind))) in
       let (names, new_binds_setup) = List.split (List.map processBind binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (body_ans, (BLetRec (List.combine names new_binds)) :: body_setup)
    | ELambda(args, body, tag) ->
       let processBind bind =
         match bind with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a lambda: %s"
                                             (string_of_bind bind))) in
       (CLambda((tag_to_lambda_name tag), List.map processBind args, helpA body, ()), [])
    | ELet((BTuple(binds, _), exp, _)::rest, body, pos) ->
       raise (InternalCompilerError("Tuple bindings should have been desugared away"))
    | EApp(func, args, native, _) ->
       let (func_ans, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(func_ans, new_args, native, ()), func_setup @ List.concat new_setup)

    | ESeq(e1, e2, _) ->
       let (e1_ans, e1_setup) = helpC e1 in
       let (e2_ans, e2_setup) = helpC e2 in
       (e2_ans, e1_setup @ [BSeq e1_ans] @ e2_setup)

    | ETuple(args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CTuple(new_args, ()), List.concat new_setup)
    | EGetItem(tup, idx, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (CGetItem(tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem(tup, idx, newval, _) ->
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (new_imm, new_setup) = helpI newval in
       (CSetItem(tup_imm, idx_imm, new_imm, ()), tup_setup @ idx_setup @ new_setup)
         

    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * unit anf_bind list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | ENil _ -> (ImmNil(), [])

    | ESeq(e1, e2, _) ->
       let (e1_imm, e1_setup) = helpI e1 in
       let (e2_imm, e2_setup) = helpI e2 in
       (e2_imm, e1_setup @ e2_setup)


    | ETuple(args, tag) ->
       let tmp = sprintf "tup_%d" tag in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [BLet (tmp, CTuple(new_args, ()))])
    | EGetItem(tup, idx, tag) ->
       let tmp = sprintf "get_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ [BLet (tmp, CGetItem(tup_imm, idx_imm, ()))])
    | ESetItem(tup, idx, newval, tag) ->
       let tmp = sprintf "set_%d" tag in
       let (tup_imm, tup_setup) = helpI tup in
       let (idx_imm, idx_setup) = helpI idx in
       let (new_imm, new_setup) = helpI newval in
       (ImmId(tmp, ()), tup_setup @ idx_setup @ new_setup @ [BLet (tmp, CSetItem(tup_imm, idx_imm, new_imm,()))])

    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [BLet (tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(And, left, right, tag) -> 
    let tmp = sprintf "binop_%d" tag in
    let (left_imm, left_setup) = helpI left in
    (ImmId(tmp, ()), left_setup @ [BLet(tmp, CLPrim2(And, left_imm, (helpA right), ()))])
  | EPrim2(Or, left, right, tag) -> 
    let tmp = sprintf "binop_%d" tag in
    let (left_imm, left_setup) = helpI left in
    (ImmId(tmp, ()), left_setup @ [BLet(tmp, CLPrim2(Or, left_imm, (helpA right), ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [BLet (tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [BLet (tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(func, args, native, tag) ->
       let tmp = sprintf "app_%d" tag in
       let (new_func, func_setup) = helpI func in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), func_setup @ (List.concat new_setup) @ [BLet (tmp, CApp(new_func, new_args, native, ()))])
    | ELet([], body, _) -> helpI body
    | ELet((BBlank _, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BSeq exp_ans] @ body_setup)
    | ELetRec(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let processBind (bind, rhs, _) =
         match bind with
         | BName(name, _, _) -> (name, helpC rhs)
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a let-rec: %s"
                                             (string_of_bind bind))) in
       let (names, new_binds_setup) = List.split (List.map processBind binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (ImmId(tmp, ()), (List.concat new_setup)
                        @ [BLetRec (List.combine names new_binds)]
                        @ body_setup
                        @ [BLet(tmp, body_ans)])
    | ELambda(args, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let processBind bind =
         match bind with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError(sprintf "Encountered a non-simple binding in ANFing a lambda: %s"
                                             (string_of_bind bind))) in
       (ImmId(tmp, ()), [BLet(tmp, CLambda((tag_to_lambda_name tag), (List.map processBind args), helpA body, ()))])
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELet((BTuple(binds, _), exp, _)::rest, body, pos) ->
       raise (InternalCompilerError("Tuple bindings should have been desugared away"))
  and helpA e : unit aexpr = 
    let (ans, ans_setup) = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(name, exp) -> ALet(name, exp, body, ())
        | BLetRec(names) -> ALetRec(names, body, ()))
      ans_setup (ACExpr ans)
  in
  helpP p
;;


(* ENVIRONMENT CREATION AND REGISTER ALLOCATION ------------------- *)
let remove_dups (vars_list : string list) : string list =
  let rec remove_duplicates_aux acc set = function
    | [] -> List.rev acc
    | hd :: tl ->
        if StringSet.mem hd set then
          remove_duplicates_aux acc set tl
        else
          remove_duplicates_aux (hd :: acc) (StringSet.add hd set) tl
  in
  remove_duplicates_aux [] StringSet.empty vars_list
;;

let free_vars (e: 'a aexpr) : string list =
  let rec helpA (e: 'a aexpr) (ctx: string list): string list =
    (match e with
      | ASeq(p1, p2, _) ->
        (helpC p1 ctx) @ (helpA p2 ctx)
      | ALet(name, bound_expr, body, _) -> 
        let bound_free_vars = helpC bound_expr ctx in
        let body_free_vars = helpA body (ctx @ [name]) in
        bound_free_vars @ body_free_vars
      | ALetRec(bindings, body, _) ->
        let (names, exprs) = List.split bindings in
        let bound_free_vars = List.concat (List.map (fun e -> helpC e ctx) exprs) in
        let body_free_vars = helpA body (ctx @ names) in
        bound_free_vars @ body_free_vars
      | ACExpr(expr) -> helpC expr ctx)
  and helpC (e: 'a cexpr) (ctx: string list): string list =
    (match e with
      | CIf(cond, t_b, e_b, _) ->
        let cond_free_vars =  helpI cond ctx in
        let t_b_free_vars = helpA t_b ctx in
        let e_b_free_vars = helpA e_b ctx in
        cond_free_vars  @ t_b_free_vars @ e_b_free_vars
      | CPrim1(_, expr, _) -> helpI expr ctx
      | CPrim2(_, expr1, expr2, _) -> helpI expr1 ctx @ helpI expr2 ctx
      | CLPrim2(_, expr1, expr2, _) -> helpI expr1 ctx @ helpA expr2 ctx
      | CApp(func, arg_exprs, Native, _) -> 
        (List.concat (List.map (fun e -> helpI e ctx) arg_exprs)) @ helpI func ctx
      | CApp(fun_expr, arg_exprs, _, _) ->
        helpI fun_expr ctx @ List.concat (List.map (fun e -> helpI e ctx) arg_exprs)
      | CImmExpr(expr) -> helpI expr ctx
      | CTuple(exprs, _) -> List.concat (List.map (fun e -> helpI e ctx) exprs)
      | CGetItem(tup_expr, idx_expr, _) -> helpI tup_expr ctx @ helpI idx_expr ctx
      | CSetItem(tup_expr, idx_expr, val_expr, _) ->
        helpI tup_expr ctx @ helpI idx_expr ctx @ helpI val_expr ctx
      | CLambda(name, params, body, _) -> (helpA body (ctx @ params)))
  and helpI (e: 'a immexpr) (ctx: string list): string list = 
    (match e with
      | ImmNum(_, _) -> []
      | ImmBool(_, _) -> []
      | ImmId(name, _) -> if List.exists (fun s -> s = name) ctx then [] else [name]
      | ImmNil(_) -> [])
  in
  remove_dups (helpA e [])
;;

(* Functions which wrap around free_vars, and return a set of strings *)
let list_to_set l = 
  List.fold_left (fun acc str -> StringSet.add str acc) StringSet.empty l
let free_vars_a (e : 'a aexpr) : StringSet.t = 
  (list_to_set (free_vars e));;
let free_vars_c (e : 'a cexpr) : StringSet.t = 
  free_vars_a (ACExpr(e));;
let free_vars_imm (e : 'a immexpr) : StringSet.t = 
  free_vars_c (CImmExpr(e));;

let free_vars_cache (prog: 'a aprogram): StringSet.t aprogram =
  let rec free_vars_cache_a (top: 'a aexpr) =
    match top with
      | ASeq(f, s, tag) -> ASeq(free_vars_cache_c f, free_vars_cache_a s, (free_vars_a top))
      | ALet(name, bound_expr, body, _) -> ALet(name, (free_vars_cache_c bound_expr), (free_vars_cache_a body), (free_vars_a top))
      | ALetRec(bindings, body, _) -> let new_b = List.map (fun (str, expr) -> (str, (free_vars_cache_c expr))) bindings in
        ALetRec(new_b, (free_vars_cache_a body), (free_vars_a top))
      | ACExpr(cexpr) -> ACExpr(free_vars_cache_c cexpr)
  and free_vars_cache_c (top: 'a cexpr): StringSet.t cexpr =
    match top with
    | CIf(cond, t, e, _) -> CIf((free_vars_cache_imm cond), (free_vars_cache_a t), (free_vars_cache_a e), (free_vars_c top))
    | CPrim1(prim, imm, _) -> CPrim1(prim, (free_vars_cache_imm imm), (free_vars_c top))
    | CPrim2(prim1, l , r, _) -> CPrim2(prim1, (free_vars_cache_imm l), (free_vars_cache_imm r), (free_vars_c top))
    | CLPrim2(prim2, l, r_aexpr, _) -> CLPrim2(prim2, (free_vars_cache_imm l), (free_vars_cache_a r_aexpr), (free_vars_c top))
    | CApp(fun_imm, arg_list, call_type, _)
       -> CApp((free_vars_cache_imm fun_imm), (List.map free_vars_cache_imm arg_list), call_type, (free_vars_c top))
    | CImmExpr(imm) -> CImmExpr(free_vars_cache_imm imm)
    | CTuple(l, _) -> CTuple((List.map free_vars_cache_imm l), (free_vars_c top))
    | CGetItem(a, b, _) -> CGetItem((free_vars_cache_imm a), (free_vars_cache_imm b), (free_vars_c top))
    | CSetItem(a, b, c, _) -> CSetItem((free_vars_cache_imm a), (free_vars_cache_imm b), (free_vars_cache_imm c), (free_vars_c top))
    | CLambda(name, params, body, _) -> CLambda(name, params, (free_vars_cache_a body), (free_vars_c top))
  and free_vars_cache_imm (top: 'a immexpr) : StringSet.t immexpr = 
    match top with
    | ImmNum (num, _) -> ImmNum(num, (free_vars_imm top))
    | ImmBool (b, _) -> ImmBool(b, (free_vars_imm top))
    | ImmId (str, _) -> ImmId(str, (free_vars_imm top))
    | ImmNil (_) -> ImmNil(free_vars_imm top) in
  match prog with 
    | AProgram(a, _) -> AProgram((free_vars_cache_a a), StringSet.empty)
;;

let interfere (e : StringSet.t aexpr) (live : StringSet.t) : grapht =
  raise (NotYetImplemented "Generate interference graphs from expressions for racer")
;;


let highest_neighbor (node : string) (g : grapht) (a : int name_envt) : int = 
  let neighbs = (match (Graph.find_opt node g) with Some(n) -> n | None -> failwith "miss") in
  let highest =
  (List.fold_left
  (fun acc (cur_name, cur_int) -> if (StringSet.mem cur_name neighbs) && cur_int > acc then cur_int else acc) 
  0
  a) in
  highest + 1

let rec lowest_available_help (node : string) (g : grapht) (a : int name_envt) (cur : int) : int = 
  let have_this = List.filter (fun (n2, num) -> (num = cur) && (has_edge g node n2)) a in
  if (List.length have_this) = 0 then cur else lowest_available_help node g a (cur + 1)

let lowest_available (node : string) (g : grapht) (a : int name_envt) : int = 
  lowest_available_help node g a 1

let assign_colors_num (g : grapht) (wl : string list) : int name_envt= 
  let ne : int name_envt = [] in
  (List.fold_left
  (fun ne (cur : string)-> let (new_num : int) = (lowest_available cur g ne) in ne @ [(cur, new_num)])
  ne
  wl);;

let regs_to_alloc = [Reg R12 ; Reg R13; Reg R14 ; Reg RBX ; Reg R8; Reg R9];;

let callee_save_setup = 
  List.map (fun arg -> (IPush arg)) regs_to_alloc;;

let callee_save_teardown = 
  List.rev_map (fun arg -> (IPop arg)) regs_to_alloc;;
let rtal = List.length regs_to_alloc;;

let num_to_arg num : Assembly.arg =
  if (num <= rtal) then (List.nth regs_to_alloc (num - 1)) else (RegOffset((~-8 * (num - rtal)), RBP))

let colors_num_to_args (init : int name_envt) : arg name_envt = 
  List.map 
  (fun (str, num) -> (str, (num_to_arg num)))
  init

let color_graph (g: grapht) (init_env: arg name_envt) : arg name_envt =
  let worklist = create_worklist g in
  let num_colors = assign_colors_num g worklist in
  let num_args = colors_num_to_args num_colors in
  let overall_env = num_args @ init_env in
  (* print_env overall_env arg_to_asm; *) 
  overall_env
;;

let remove_elements a b =
  let is_not_in_a x = not (List.mem x a) in
  List.filter is_not_in_a b
;;

let other_let_bounds expr = 
  let rec help_c e = 
    match e with
    | CIf(_, l, r, _) -> (help_a l) @ (help_a r)
    | CLPrim2(_, _, e, _) -> (help_a e)
    | _ -> []
  and help_a e = 
    match e with
    | ASeq (_, a, _) -> help_a a
    | ALet(str, c, body, _) -> [str] @ (help_c c) @ (help_a body)
    | ALetRec(binds, body, _) -> (fst (List.split binds)) @ (List.concat_map help_c (snd (List.split binds))) @ (help_a body)
    | ACExpr(c) -> (help_c c) in
  help_c expr
  

(* TODO: change signature and implmenetaiton*)

(* returns: Current ENV, Global ENV*)
let rec ra_generator (e : StringSet.t aexpr) (g_init : grapht): (grapht * (grapht * arg name_envt) name_envt) = 
  match e with
    | ALet(name, bound, body, frees) -> 
      let (env_current, env_overall) = ra_generator body (add_node g_init name) in
      let (bound_current, bound_overall) = ra_generator_c bound g_init in
      let updated_body_current = make_connections name (get_tag_A body) env_current in
      let hidden_binds = other_let_bounds bound in
      let more_connections = List.fold_left (fun acc cur -> make_connections cur (get_tag_A body) acc) updated_body_current hidden_binds in
      let combined_current = (append_graphs more_connections bound_current) in
      (combined_current, env_overall @ bound_overall)
    | ALetRec(binds, body, _) -> 
      let (names, bound_exprs) = List.split binds in
      let g_init = List.fold_left (fun gacc (str, bound) -> (add_node gacc str)) g_init binds in
      let w_body = ra_generator body g_init in
      let (c, o) = List.fold_left (fun (c, o) expr -> let (lc, lo) = (ra_generator_c expr g_init) in (append_graphs lc c, o @ lo)) w_body bound_exprs in
      (* make connection between each bind names and anything free in and of the binds or body*)
      let all_frees = List.fold_left (fun sacc (str, bound) -> StringSet.union sacc (get_tag_C bound)) (get_tag_A body) binds in
      let updated_graph = List.fold_left (fun gacc name -> make_connections name all_frees gacc) c names in
      (updated_graph, o)
    | ASeq (c, a, frees) -> 
        let (cc, co) = ra_generator_c c g_init in
        let (ac, ao) = ra_generator a g_init in
        (append_graphs ac cc, co @ ao)
    | ACExpr(CLPrim2(a, b, c, d)) -> ra_generator_c (CLPrim2(a, b, c, d)) g_init 
    | ACExpr(CIf(a, b, c, d)) -> ra_generator_c (CIf(a, b, c, d)) g_init 
    | ACExpr(CLambda(name, params, body, tag)) -> let lam = CLambda(name, params, body, tag) in ra_generator_c lam g_init 
    | _ -> (g_init, [])
and ra_generator_c (e : StringSet.t cexpr) (g_init : grapht)  : (grapht * (grapht * arg name_envt) name_envt) = 
  match e with
    | CLambda(name, params, body, free_set) ->
      let frees = (StringSet.elements free_set) in
      let frees_minus_params = remove_elements params frees in
      let (ec, eo) = ra_generator body (add_nodes frees_minus_params Graph.empty) in
      let current = (remove_nodes params ec) in
      let param_env = cmap (fun name cnt -> (name, (RegOffset(24 + 8 * cnt, RBP)))) params in
      (g_init, eo @ [(name, (current, param_env))])
    | CLPrim2(_, _, r, _) -> ra_generator r g_init
    | CIf(_, t, e, _) ->
      let (t_curr, t_tot) = ra_generator t g_init in
      let (e_curr, e_tot) = ra_generator e g_init in
      let currEnv = append_graphs t_curr e_curr in
      (currEnv, t_tot @ e_tot)
    | _ -> (g_init, []);;
;;

let print_env (environment : arg name_envt name_envt) = 
  (printf "\nhere is the environment: \n") ;
  List.map (fun (lam, env) -> 
    (List.map (fun (str, arg) -> (printf "In lambda %s, the id %s is mapped to the arg %s\n" lam str (arg_to_asm arg))) env))
    environment

let rec arg_envt_generator (e : tag aexpr) (counter : int): (arg name_envt * arg name_envt name_envt) = 
  match e with
    | ALet(name, bound, body, _) -> 
      let (env_current, env_overall) = arg_envt_generator body (counter + 1) in
      let (bound_current, bound_overall) = arg_envt_generator_c bound counter in
      (env_current @ bound_current @ [(name, RegOffset((~- 8 * counter), RBP))], env_overall @ bound_overall)
    | ALetRec(binds, body, _) -> 
      let top_level_binds_body = (cmap (fun (name, expr) idx -> (name, RegOffset((~- 8 * (counter + idx)), RBP))) binds) in
      let (env_current, env_overall) = arg_envt_generator body (counter + List.length binds) in
      let (lambdas_bc, lambdas_bo) =
      (List.fold_left 
        (fun (acc_bo, acc_bc) (_, current_expr) -> 
          (let (new_bo, new_bc) = (arg_envt_generator_c current_expr counter) in
            (acc_bo @ new_bo, acc_bc @ new_bc)))
        ([], [])
        binds) in 
        (env_current @ lambdas_bc @ top_level_binds_body, env_overall @ lambdas_bo)
    | ASeq (c, a, _) -> let (cc, co) = arg_envt_generator_c c counter in
        let (ac, ao) = arg_envt_generator a counter in
        (cc @ ac, co @ ao)
    | ACExpr(CLPrim2(a, b, c, d)) -> arg_envt_generator_c (CLPrim2(a, b, c, d)) counter 
    | ACExpr(CIf(a, b, c, d)) -> arg_envt_generator_c (CIf(a, b, c, d)) counter 
    | ACExpr(CLambda(name, params, body, tag)) -> let lam = CLambda(name, params, body, tag) in arg_envt_generator_c lam counter 
    | _ -> ([], [])
and arg_envt_generator_c (e : tag cexpr) (counter : int)  : (arg name_envt * arg name_envt name_envt) = 
  match e with
    | CLambda(name, params, body, tag) ->
      let lambda_name = name in
      let frees_with_params = (free_vars body) in
      let frees = remove_elements params frees_with_params in
      let param_env = (cmap (fun name i -> (name, RegOffset(24 + (i * 8), RBP))) params) in
      let free_env = (cmap (fun name i -> (name, RegOffset(~-8 * (i + 1), RBP))) frees) in
      let (ec, eo) = arg_envt_generator body (List.length frees + 1) in
      let current = param_env @ free_env @ ec in
      ([], eo @ [(lambda_name, current)])
    | CLPrim2(_, _, r, _) -> arg_envt_generator r counter
    | CIf(_, t, e, _) ->
      let (t_curr, t_tot) = arg_envt_generator t counter in
      let (e_curr, e_tot) = arg_envt_generator e counter in
      let currEnv = t_curr @ e_curr in
      (currEnv, t_tot @ e_tot)
    | _ -> ([], [])
;;

(* we chose this signature because we had already implemented it this way for the
   previous assignment *)
let naive_stack_allocation (prog: tag aprogram): tag aprogram * arg name_envt name_envt =
  let AProgram(expr, _) = prog in
  let (local_env, global_env) = arg_envt_generator expr 1 in
  let overall_env = (global_env @ [("?our_code_starts_here", local_env)]) in
  (prog, overall_env)
;;
let register_allocation (prog: tag aprogram) : tag aprogram * arg name_envt name_envt =
  let fv_prog = (free_vars_cache prog) in
  let AProgram(expr, _) = fv_prog in
  let (local_env, global_env) = ra_generator expr Graph.empty in
  let colored = color_graph local_env [] in
  let global_solved = List.map (fun (str, (g, params)) -> (str, (color_graph g params))) global_env in
  let overall_env = (global_solved @ [("?our_code_starts_here", colored)]) in 
  (prog, overall_env);;

(* ------------------------- COMPILATION ---------------------------- *)
let make_section_start_label section_name =
  ILineComment(section_name ^ " START ---------------------")

let make_section_end_label section_name =
  ILineComment(section_name ^ " END ^^^^^^^^^^^^^^^^^^^^^^")

let make_error_label_section (err: error_type) : instruction list = 
  [ILabel(error_type_to_string err);
  IMov(Reg RSI, Reg RAX);
  IMov (Reg RDI, (Const (error_type_to_code err)));
  ICall (Label("error"))]
;;

(* Expects decoded tuple value in RAX and machine num index value in R11 *)
let make_idx_oob_check oob_type: instruction list =
  (* swap registers so Index is in RAX (for error message) *)
  [make_section_start_label "IDX OOB";
  IMov (Reg R10, Reg RAX);
  IMov (Reg RAX, Reg R11);
  IMov (Reg R11, Reg R10)] @
  (* r10: encoded length, rax: encoded index *)
  [IMov (Reg R10, (RegOffset(0, R11)));
  ISar (Reg R10, Const(Int64.one)) ;
  IMul (Reg RAX, (Const (Int64.of_int 2)));
  IMul (Reg R10, (Const (Int64.of_int 2)));
  (* perform the checks *)
  ICmp (Reg RAX, Reg R10);
  IJge (Label(error_type_to_string (if oob_type = "get" then GET_HIGH_IDX else SET_HIGH__IDX)));
  ICmp (Reg RAX, (Const (Int64.zero)));
  IJl (Label(error_type_to_string (if oob_type = "get" then GET_LOW_IDX else SET_LOW__IDX)));
  (* decode RAX *)
  ISar (Reg RAX, (Const (Int64.one)))]
   @
  (* swap back *)
  [IMov (Reg R10, Reg RAX);
  IMov (Reg RAX, Reg R11);
  IMov (Reg R11, Reg R10);
  make_section_end_label "IDX OOB"]
;;

let make_bool_type_check (err: error_type) : instruction list =
  [ITest(Reg RAX, HexConst(num_tag_mask));
  (IMov (Reg(scratch_reg), Reg RAX));
  IJz(Label(error_type_to_string err))]
;;

let make_num_type_check (err: error_type) : instruction list =
  [ITest(Reg RAX, HexConst(num_tag_mask));
  IMov (Reg(scratch_reg), Reg RAX);
  IJnz(Label(error_type_to_string err))]
;;

let make_overflow_check : instruction list = 
  [IJo (Label(error_type_to_string OVERFLOW))]
;;

let make_tuple_check tup_fun_type: instruction list =
  [IAnd(Reg RAX, HexConst(bool_tag_mask));
  ICmp(Reg RAX, HexConst(1L));
  IJne(Label(error_type_to_string (if tup_fun_type = "get" then GET_NOT_TUPLE else SET_NOT_TUPLE)))]
;;

let compile_tuple elts =
  let length = List.length elts in
  let length_plus1 = length + 1 in
  let odd_bool = isOdd(length_plus1) in
  let even_length = (round_even length_plus1) in
  (* Allocate the length as SNAKE number *)
  [IMov(Reg R11, (Const (Int64.of_int(2 * length)))); IMov (RegOffset(0, R15), Reg R11)] @
  (* allocate elements *)
  fst (List.fold_left
    (fun (instrs, cnt) elt ->
      (instrs @ [IMov(Reg R11, elt); IMov(RegOffset(8 * cnt, R15), Reg R11)]), cnt + 1)
    ([], 1)
    elts) @
  (if odd_bool then [IMov(Reg R11,  Const(Int64.zero)) ;
    IMov((RegOffset((8 * length_plus1), R15)), Reg R11)] else []) @
  [IMov(Reg RAX, Reg R15)] @
  [IAdd(Reg RAX, HexConst(1L))] @
  (* Move up R15 *)
  [IMov(Reg R10, (Const (Int64.of_int (even_length * 8))))] @
  [IAdd(Reg R15, Reg R10)]
;; 

(* ERROR: Tuple IDX stuff*)
(* Moves tuple and idx args into RAX and R11 respectively. *)
let decode_tuple_idx_pair tuple idx =
  [IMov(Reg RAX, tuple);
  ISub(Reg RAX, Const(Int64.one));
  IMov(Reg R11, idx);
  ISar(Reg R11, Const(Int64.one))]
;;

let make_tuple_pair_type_check tuple idx tup_fun_type =
  [IMov (Reg RAX, tuple)] @
  make_tuple_check tup_fun_type @
  [IMov (Reg RAX, idx)] @
  make_num_type_check (if tup_fun_type = "get" then ARITH_NOT_NUM else ARITH_NOT_NUM)
;;

let compile_get_item tuple idx =
  make_tuple_pair_type_check tuple idx "get"@
  decode_tuple_idx_pair tuple idx @
  make_idx_oob_check "get" @
  [IMov (Reg RAX, RegOffsetReg(RAX, R11, 8, 8))]
;;

let compile_set_item tuple idx value=
  make_tuple_pair_type_check tuple idx "set" @
  decode_tuple_idx_pair tuple idx @
  make_idx_oob_check "set" @
  [IMov(Reg R10, value);
  IMov (RegOffsetReg(RAX, R11, 8, 8), Reg R10)]
;;
let rec find_closure_env (env: arg name_envt name_envt) (lam_name: string) (id: string) =
  match env with
  | [] -> raise (InternalCompilerError (sprintf "Lambda %s not found" lam_name))
  | (y,v)::rest -> 
      if y = lam_name then find_closure_aux lam_name v id else find_closure_env rest lam_name id
and find_closure_aux lam_name (env: arg name_envt) (id: string) =
  match env with
  | [] -> raise (InternalCompilerError (sprintf "Lambda %s but, Name %s not found" lam_name id))
  | (y,v)::rest ->
      if y = id then v else find_closure_aux lam_name rest id
;;
let make_lambda_type_check = 
  [make_section_start_label "LAMBDA TYPE CHECK";
  IAnd(Reg RAX, HexConst(bool_tag_mask));
  ICmp(Reg RAX, HexConst(lambda_tag));
  IJne(Label(error_type_to_string CALL_NOT_CLOSURE));
  make_section_end_label "LAMBDA TYPE CHECK"]

  (* TAGGED FUNCTION IN RAX *)
let make_arity_check len_const =
  [make_section_start_label "ARITY CHECK";
  IAdd (Reg RAX, Const((Int64.of_int(~-5))))] @
  [IMov(Reg R10, Const(Int64.of_int(len_const)))] @
  [IMov(Reg R11, RegOffset(0, RAX))] @
  (* Convert to MACHINEVAL *)
  [ISar(Reg R11, Const(Int64.one))] @
  [ICmp(Reg R11, Reg R10)] @
  [IJne(Label(error_type_to_string CALL_ARITY));
  make_section_end_label "ARITY CHECK"]

  let compile_short_circuit (c : arg) (t : instruction list) (e : instruction list) (tag : tag) : instruction list  =
  [IMov (Reg RAX, c)] @
  (make_bool_type_check IF_NOT_BOOL) @
  [ICmp (Reg RAX, const_true)] @
  [IJe (Label("thencase#" ^ (string_of_int tag)))] @ 
  e @
  [IJmp (Label("end#" ^ (string_of_int tag)))] @
  [ILabel ("thencase#" ^ (string_of_int tag))] @
  t @  
  [ILabel ("end#" ^ (string_of_int tag))]
;;

let compile_cprim1 (op : prim1) (arg : arg) (tag : tag) : instruction list =
  [IMov(Reg RAX, arg)] @
  (match op with
        | Add1 -> make_num_type_check ARITH_NOT_NUM @ [IAdd(Reg RAX, HexConst(2L))] @ make_overflow_check
        | Sub1 -> make_num_type_check ARITH_NOT_NUM @ [IAdd(Reg RAX, HexConst(-2L))] @ make_overflow_check
        | Print -> [IMov (Reg RDI, Reg RAX); ICall(Label("print"))]
        | IsBool -> 
            [IAnd(Reg RAX, HexConst(bool_tag_mask));
            ICmp(Reg RAX, HexConst(7L));
            IJne(Label("isBooly#" ^ (string_of_int tag)));
            IMov(Reg RAX, const_true);
            IJmp(Label("end#" ^ (string_of_int tag)));
            ILabel("isBooly#" ^ (string_of_int tag));
            IMov(Reg RAX, const_false);
            ILabel("end#" ^ (string_of_int tag))]
        | IsNum -> 
          [ITest(Reg RAX, HexConst(num_tag_mask));
          IJnz(Label("isNumy#" ^ (string_of_int tag)));
          IMov(Reg RAX, const_true);
          IJmp(Label("end#" ^ (string_of_int tag)));
          ILabel("isNumy#" ^ (string_of_int tag));
          IMov(Reg RAX, const_false);
          ILabel("end#" ^ (string_of_int tag))]
        | Not ->
          make_bool_type_check LOGIC_NOT_BOOL @ 
          [IMov(Reg R11, bool_mask); IXor(Reg RAX, Reg R11)]
        | PrintStack -> failwith "Unexpected PrintStack"
        | IsTuple ->
          [IAnd(Reg RAX, HexConst(bool_tag_mask));
          ICmp(Reg RAX, HexConst(1L));
          IJne(Label("isTupy#" ^ (string_of_int tag)));
          IMov(Reg RAX, const_true);
          IJmp(Label("end#" ^ (string_of_int tag)));
          ILabel("isTupy#" ^ (string_of_int tag));
          IMov(Reg RAX, const_false);
          ILabel("end#" ^ (string_of_int tag))])
 ;;
 
let compile_cprim2_cnd (op : prim2) (l : arg) (r : arg) (tag : tag) : instruction list = 
  [IMov (Reg RAX, l)] @
  [IMov (Reg R11, r)] @
  [ISar (Reg RAX, Const(1L))] @
  [ISar (Reg R11, Const(1L))] @
  [ICmp (Reg RAX, Reg R11)] @
  (match op with
    | Greater -> [IJg(Label("truecase#" ^ (string_of_int tag)))]
    | GreaterEq -> [IJge(Label("truecase#" ^ (string_of_int tag)))]
    | Less -> [IJl(Label("truecase#" ^ (string_of_int tag)))]
    | LessEq ->[IJle(Label("truecase#" ^ (string_of_int tag)))]
    | _ -> failwith "Unexpectd non-cond prim2 in compile_cprim2_cnd") @
  [IMov(Reg RAX, const_false)] @
  [IJmp(Label("end#" ^ (string_of_int tag)))] @
  [ILabel("truecase#" ^ (string_of_int tag))] @
  [IMov(Reg RAX, const_true)] @
  [ILabel("end#" ^ (string_of_int tag))];;
;;

let compile_cprim2_num (op : prim2) (l : arg) (r : arg) (tag : tag) : instruction list = 
  (* type checks, then moves l and r to RAX and R11 *)
  [IMov(Reg RAX, l)]
  @ make_num_type_check ARITH_NOT_NUM
  @ [IMov(Reg RAX, r)]
  @ (make_num_type_check ARITH_NOT_NUM)
  @ [IMov(Reg RAX, l)] 
  @ [IMov(Reg R11, r)] @
  (match op with
    | Plus -> [IAdd (Reg RAX, Reg R11)]
            @ (make_overflow_check)
    | Minus -> [ISub (Reg RAX, Reg R11)]
            @ (make_overflow_check)
    | Times -> 
      [IMov (Reg RAX, l)] @
      [IMov (Reg R11, r)] @
      [ISar (Reg RAX, Const(1L))] @
      [ISar (Reg R11, Const(1L))] @
      [IMul (Reg RAX, Reg R11)] @
      (make_overflow_check) @
      [IMul (Reg RAX, Const(2L))] @
      (make_overflow_check)
    | Greater -> compile_cprim2_cnd op l r tag
    | GreaterEq -> compile_cprim2_cnd op l r tag
    | Less -> compile_cprim2_cnd op l r tag
    | LessEq -> 
      compile_cprim2_cnd op l r tag
    | _ -> failwith "Unexpected bool expr in compile_cprim2_num")
;;

let compile_cprim2 (op : prim2) (l : arg) (r : arg) (tag : tag) : instruction list =
  match op with
  | Plus -> compile_cprim2_num op l r tag
  | Minus -> compile_cprim2_num op l r tag
  | Times -> compile_cprim2_num op l r tag
  | And -> failwith "shouldn't get here, handled this earlier"
  | Or -> failwith "shouldn't get here, handled this earlier"
  | Greater -> compile_cprim2_num op l r tag
  | GreaterEq -> compile_cprim2_num op l r tag
  | Less -> compile_cprim2_num op l r tag
  | LessEq -> compile_cprim2_num op l r tag
  | CheckSize -> []
  | Eq ->
      [IMov (Reg RAX, l)] @
      [ICmp (Reg RAX, r)] @
      [IJnz (Label("falsecase#" ^ (string_of_int tag)))] @
      [IMov (Reg RAX, r)] @ 
      [ICmp (Reg RAX, l)] @
      [IJnz (Label("falsecase#" ^ (string_of_int tag)))] @
      [IMov (Reg RAX, const_true)] @
      [IJmp (Label("end#" ^ (string_of_int tag)))] @
      [ILabel ("falsecase#" ^ (string_of_int tag))] @
      [IMov (Reg RAX, const_false)] @
      [ILabel ("end#" ^ (string_of_int tag))]
;;


let compile_clprim2 op l r tag = 
  let ri = r @ make_bool_type_check LOGIC_NOT_BOOL in
  [IMov (Reg RAX, l)] @ 
  make_bool_type_check LOGIC_NOT_BOOL @ 
  if op = And then (compile_short_circuit l ri [IMov(Reg RAX, const_false)] tag) else 
                  (compile_short_circuit l [IMov(Reg RAX, const_true)] ri tag)
;;

let function_padded_length params body = 
  let frees_with_params = (free_vars body) in
  let frees = remove_elements params frees_with_params  in
  let naked_length = 3 + (List.length frees) in 
  (round_even naked_length);;
(* p1: stack setup (for body)
  p3: stack teardown (for body)
  p4: closure setup *)
let compile_function_outsides params body (env: arg name_envt name_envt) (lam_name : string) (old_lam_name : string)
  : (instruction list * instruction list * instruction list) =
  (* free length gets the param length subtracted from it*)
  let frees_with_params = (free_vars body) in
  let frees = remove_elements params frees_with_params  in
  let free_length = List.length(frees) in
  let naked_length = 3 + (List.length frees) in 
  let padded = (round_even naked_length) in
  let counted_vars = count_vars body in 
  (* 16 byte allignment *)
  let counted_plus = counted_vars + free_length in 
  let even_stack_offset = (round_even counted_plus) in
  (* function body *)
  let p1 = 
  ([(IJmp(Label(lam_name ^ "end")))] @
  [ILabel lam_name] @
  [(IPush(Reg RBP))] @ 
  [IMov(Reg RBP, Reg RSP)] @ 
  (* move up RSP*)
  List.init (even_stack_offset) (fun _ -> IPush(pw)) @
  (* get fun object address in r11 SHOULD BE UNTAGGED *)
  [IMov(Reg R11, RegOffset(16, RBP))] @
  (* push the things there to stack *)
  (cmap_star (fun imm idx -> [IMov(Reg R10, RegOffset(idx * 8 + 24, R11));
                                  IMov((find_closure_env env lam_name imm), Reg R10)]) frees)) in
  let p3 =
  ([IMov (Reg RSP, Reg RBP)] @
  [IPop (Reg RBP)] @
  [IRet] @
  [(ILabel(lam_name ^ "end"))]) in
  let p4 = 
  (* closure setup *)
  (* make_oom_check padded @ *)
  (* store arity as SNAKE number *)
  [IMov(Reg R10, Const(Int64.of_int(2 * List.length(params))))] @
  [IMov(RegOffset(0, R15), Reg R10)] @ 
  (* store instruction pointer *)
  [ILea (Reg R10, lam_name)] @
  [IMov(RegOffset(8, R15), Reg R10)] @
  (* SNAKE size *)
  [IMov(Reg R10, Const(Int64.of_int(2 * free_length)))] @
  [IMov(RegOffset(16, R15), Reg R10)] @
  cmap_star (fun free idx -> 
      [(IMov(Reg R10, (find_closure_env env old_lam_name free))) ;
      (IMov(RegOffset(24 + idx * 8, R15), Reg R10))]) frees @
  [IMov(Reg RAX, Reg R15)] @ 
  [IAdd(Reg RAX, Const(lambda_tag))] @
  [IAdd(Reg R15, Const(Int64.of_int(8 * padded)))]
  in (p1, p3, p4)
 

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))
and reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  [
    IInstrComment(ILea(Reg(RAX),"?HEAP_END"),
                 sprintf "Reserving %d words" (size / word_size));
    ISub(Reg(RAX), Const(Int64.of_int size));
    ICmp(Reg(RAX), Reg(heap_reg));
    IJge(Label ok);
  ] @
  (native_call (Label "?try_gc") [
         (Sized(QWORD_PTR, Reg(heap_reg))); (* alloc_ptr in C *)
         (Sized(QWORD_PTR, Const(Int64.of_int size))); (* bytes_needed in C *)
         (Sized(QWORD_PTR, Reg(RBP))); (* first_frame in C *)
         (Sized(QWORD_PTR, Reg(RSP))); (* stack_top in C *)
    ]) 
  @ [
      IInstrComment(IMov(Reg(heap_reg), Reg(RAX)), "assume gc success if returning here, so RAX holds the new heap_reg value");
      ILabel(ok);
    ]
(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
(* Additionally, you are provided an initial environment of values that you may want to
   assume should take up the first few stack slots.  See the compiliation of Programs
   below for one way to use this ability... *)
and compile_fun lam_name old_lam_name args body env = 
  let (p1, p3, p4) = compile_function_outsides args body env lam_name old_lam_name in
  (p1, (compile_aexpr body env (List.length args) false lam_name), p3 @ p4)
and compile_function (params: string list) (body: tag aexpr) (env: arg name_envt name_envt) (lam_name : string) (old_lam_name : string)
  : instruction list =
  (* dummy values for tail call things *)
  let (setup, body, ending) = (compile_fun lam_name old_lam_name params body env) in
  setup @ body @ ending
and args_help args regs =
  match args, regs with
  | arg :: args, reg :: regs ->
    IMov(Sized(QWORD_PTR, Reg(reg)), arg) :: args_help args regs
  | args, [] ->
    List.rev_map (fun arg -> IPush arg) args
  | [], _ -> []
and native_call label args =
  (* We know that on entry to every function, RSP is 16-byte aligned.
     We know that every frame is a multiple of 16 bytes.
     The call instruction pushes one return pointer onto the stack.
     The first thing we do is push RBP onto the stack
     So, we add 8 bytes of padding IFF the number of spilled args is *ODD*.
  *)
  let num_stack_args = max (List.length args - 6) 0 in
  let padding_needed = (num_stack_args mod 2) <> 0 in
  let setup = (if padding_needed
               then [IInstrComment(IPush(Sized(QWORD_PTR, Const(0L))), "Padding to 16-byte alignment")]
               else []) @ args_help args first_six_args_registers in
  let teardown =
    (if num_stack_args = 0 then []
     else [ IInstrComment(IAdd(Reg(RSP), Const(Int64.of_int(word_size * num_stack_args))),
                          sprintf "Popping %d arguments" num_stack_args) ])
    @ (if padding_needed then [IInstrComment(IAdd(Reg(RSP), Const(Int64.of_int word_size)), "Unpadding one word")]
       else []) in
  callee_save_setup @ setup @ [ ICall(label) ] @ teardown @ callee_save_teardown
(* UPDATE THIS TO HANDLE FIRST-CLASS FUNCTIONS AS NEEDED -- THIS CODE WILL NOT WORK AS WRITTEN *)
and call (fun_imm : arg) args =
  callee_save_setup @
  (* accounting for function itself being an arg *)
  let odd_args = ((List.length args) + 1) mod 2 == 1 in
  [make_section_start_label "APP"] @
  [IMov(Reg RAX, fun_imm)] @
  make_lambda_type_check @
  (* arity check stuff *)
  [IMov(Reg RAX, fun_imm)] @
  make_arity_check (List.length(args))  @
  (if odd_args then [IPush(pw)] else []) @
  (List.fold_left 
      (fun l arg -> l @ [(IMov (Reg R11, arg)); (IPush (Reg R11))]) 
    [] (List.rev args)) @
  [IMov (Reg R11, fun_imm)] @
  (*untag*)
  [IAdd (Reg R11, Const((Int64.of_int(~-5))))] @
  [IPush (Reg R11)] @
  [ICall (RegOffset(8, R11))] @
  (* pop for function *)
  [IPop(Reg R11)] @
  (List.map (fun _ -> (IPop(Reg R11))) args) @
  (if odd_args then [IPop (Reg R11)] else []) @
  callee_save_teardown @
  [make_section_end_label "APP"]
and compile_aexpr (e : tag aexpr) (env: arg name_envt name_envt) (num_args : int) (is_tail : bool) (lam_name: string): instruction list =
  (match e with
    | ACExpr(c) -> (compile_cexpr c env num_args is_tail lam_name)
    | ALet("blank", bound, body, _) -> 
      (compile_cexpr bound env num_args is_tail lam_name) @ 
      (compile_aexpr body env num_args is_tail lam_name)
    | ALet(name, bound, body, _) -> 
      (compile_cexpr bound env num_args is_tail lam_name) @ 
      [IMov ((find_closure_env env lam_name name), Reg RAX)] @
      (compile_aexpr body env num_args is_tail lam_name)
    | ASeq(f, s, _) -> 
        let first_compiled = compile_cexpr f env num_args is_tail lam_name in
        let second_compiled = compile_aexpr s env num_args is_tail lam_name in
        first_compiled @ second_compiled
    | ALetRec(bind_list, body, tag) -> 
      let sizes = (List.map (fun (_, func)
      -> let (_, args, body, _) = (get_clambda_values func) in
        function_padded_length args body) bind_list) in
      let overall_size = List.fold_left (+) 0 sizes in
      (reserve (overall_size * 8) tag) @
      ([IMov (Reg R11, Reg R15)]) @
      (let single_bind_handle = (fun bind -> 
        (let (iname, func) = bind in
        let (_, args, body, _) = (get_clambda_values func) in
        [IMov (Reg RAX, Reg R11)] @
        [IAdd (Reg RAX, Const(closure_tag))] @
        [IAdd (Reg R11, Const(Int64.of_int(((function_padded_length args body) * 8))))] @
        [IMov ((find_closure_env env lam_name iname), Reg RAX)])) in
      List.concat(List.map single_bind_handle bind_list)) @
      List.concat((List.map (fun (_, func) -> 
        let (new_name, args, body, _) = (get_clambda_values func) in
        (compile_function args body env new_name lam_name)) bind_list)) @
      (compile_aexpr body env num_args is_tail lam_name))
and compile_cexpr (e : tag cexpr) (env: arg name_envt name_envt) (num_args: int) (is_tail: bool) (lam_name: string) =
  (match e with
  | CIf(cond, t, e, tag) -> compile_short_circuit (compile_imm cond env lam_name) 
                                                  (compile_aexpr t env num_args is_tail lam_name) 
                                                  (compile_aexpr e env num_args is_tail lam_name)
                                                  tag                
  | CPrim1(op, se, tag) -> compile_cprim1 op (compile_imm se env lam_name) tag
  | CPrim2(op, l, r, tag) -> compile_cprim2 op (compile_imm l env lam_name) (compile_imm r env lam_name) tag
  | CLPrim2(op, l, r, tag) -> compile_clprim2 op (compile_imm l env lam_name) (compile_aexpr r env num_args is_tail lam_name) tag
  | CApp(lambda, args, Snake, _) ->
    call (compile_imm lambda env lam_name)  (List.map (fun imm -> (compile_imm imm env lam_name)) args) 
  | CApp((ImmId(poof, _)), args, Native, _) -> 
    let compiled_imms = (List.map (fun imm -> (compile_imm imm env lam_name)) args) in
    fst (List.fold_left (fun (l, cnt) arg -> (l @ [IMov(Reg (List.nth first_six_args_registers cnt), arg)]), cnt + 1)
    ([], 0) compiled_imms) @ 
    callee_save_setup @
    [ICall(Label poof)] @
    callee_save_teardown
  | CApp(_, _, _, _) -> failwith "badly formatted capp"
  | CImmExpr(imm) -> [IMov(Reg RAX, (compile_imm imm env lam_name))]
  | CTuple(es, tag) ->  (reserve ((round_even ((List.length(es)) + 1)) * 8) tag) @ 
                      compile_tuple (List.map (fun expr -> compile_imm expr env lam_name) es)
  | CGetItem(tup, idx, _) -> (compile_get_item (compile_imm tup env lam_name) (compile_imm idx env lam_name))
  | CSetItem(tup, idx, value, _) -> (compile_set_item (compile_imm tup env lam_name) (compile_imm idx env lam_name) (compile_imm value env lam_name))
  | CLambda(name, params, body, tagg) -> let frees = remove_elements params (free_vars body) in
                                   (reserve ((round_even ((List.length frees) + 3)) * 8) tagg) @
    compile_function params body env name lam_name)
and compile_imm e (env: arg name_envt name_envt) (lam_name: string) =
  (match e with
  | ImmNum(n, _) -> Const(Int64.shift_left n 1)
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find_closure_env env lam_name x)
  | ImmNil(_) -> HexConst(1L))
;;

(* This function can be used to take the native functions and produce DFuns whose bodies
   simply contain an EApp (with a Native call_type) to that native function.  Then,
   your existing compilation can turn these DFuns into ELambdas, which can then be called
   as in the rest of Fer-De-Lance, but the Native EApps will do the work of actually
   native_calling the runtime-provided functions. *)
let add_native_lambdas (p : sourcespan program) =
  let wrap_native name arity =
    let argnames = List.init arity (fun i -> sprintf "%s_arg_%d" name i) in
    [DFun(name, List.map (fun name -> BName(name, false, dummy_span)) argnames, EApp(EId(name, dummy_span), List.map(fun name -> EId(name, dummy_span)) argnames, Native, dummy_span), dummy_span)]
  in
  match p with
  | Program(declss, body, tag) ->
    Program((List.fold_left (fun declss (name, (_, arity)) -> (wrap_native name arity)::declss) declss native_fun_bindings), body, tag)

let compile_prog (anfed, (env : arg name_envt name_envt)) =
  let prelude =
    "section .text
extern ?error
extern ?input
extern ?print
extern ?print_stack
extern ?equal
extern ?try_gc
extern ?naive_print_heap
extern ?HEAP
extern ?HEAP_END
extern ?set_stack_bottom
global ?our_code_starts_here" in
  let suffix = sprintf "
?err_comp_not_num:%s
?err_arith_not_num:%s
?err_logic_not_bool:%s
?err_if_not_bool:%s
?err_overflow:%s
?err_get_not_tuple:%s
?err_get_low_index:%s
?err_get_high_index:%s
?err_nil_deref:%s
?err_out_of_memory:%s
?err_set_not_tuple:%s
?err_set_low_index:%s
?err_set_high_index:%s
?err_call_not_closure:%s
?err_call_arity_err:%s
"
                       (to_asm (native_call (Label "?error") [Const(err_COMP_NOT_NUM); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_ARITH_NOT_NUM); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_LOGIC_NOT_BOOL); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_IF_NOT_BOOL); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_OVERFLOW); Reg(RAX)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_NOT_TUPLE); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_LOW_INDEX); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_GET_HIGH_INDEX)]))
                       (to_asm (native_call (Label "?error") [Const(err_NIL_DEREF); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_OUT_OF_MEMORY); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_NOT_TUPLE); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_LOW_INDEX); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_SET_HIGH_INDEX); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_CALL_NOT_CLOSURE); Reg(scratch_reg)]))
                       (to_asm (native_call (Label "?error") [Const(err_CALL_ARITY_ERR); Reg(scratch_reg)]))
  in
  match anfed with
  | AProgram(body, _) ->
  (* $heap and $size are mock parameter names, just so that compile_fun knows our_code_starts_here takes in 2 parameters *)
     let (prologue, comp_main, epilogue) = compile_fun "?our_code_starts_here" "?our_code_starts_here" ["$heap"; "$size"] body env in
     let heap_start =
       [
         ILineComment("heap start");
         IInstrComment(IMov(Sized(QWORD_PTR, Reg(heap_reg)), Reg(List.nth first_six_args_registers 0)), "Load heap_reg with our argument, the heap pointer");
         IInstrComment(IAdd(Sized(QWORD_PTR, Reg(heap_reg)), Const(15L)), "Align it to the nearest multiple of 16");
         IMov(Reg scratch_reg, HexConst(0xFFFFFFFFFFFFFFF0L));
         IInstrComment(IAnd(Sized(QWORD_PTR, Reg(heap_reg)), Reg scratch_reg), "by adding no more than 15 to it");
         (* our current hack for end of stack*)
         (IMov(Reg R13, Reg(List.nth first_six_args_registers 1)));
         (IShl (Reg R13, Const(Int64.of_int 3)));
       ] in
     let set_stack_bottom =
       [
        (* IMPORTANT: commenting this out because we return this from compile_fun*)
        (* ILabel("?our_code_starts_here"); *)
         IMov(Reg R10, Reg RDI);
        
       ]
       @ (native_call (Label "?set_stack_bottom") [Reg(RBP)])
       @ [
           IMov(Reg RDI, Reg R10)
         ] in
     let reg_safety = List.map (fun reg -> (IMov(reg, Const(Int64.zero)))) regs_to_alloc in
     let main = (prologue @ reg_safety @ set_stack_bottom @ heap_start @ comp_main @ epilogue) in
     sprintf "%s%s%s\n" prelude (to_asm main) suffix
;;

let run_if should_run f =
  if should_run then f else no_op_phase 
;;

let pick_alloc_strategy (strat : alloc_strategy) =
  match strat with
  | Naive -> naive_stack_allocation
  | Register -> register_allocation
;;


(*  TODO: set no_builtins default to false *)
let compile_to_string ?no_builtins:(no_builtins=false) (alloc_strat : alloc_strategy) (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (run_if (not no_builtins) (add_phase add_natives add_native_lambdas)) 
  |> (add_err_phase well_formed is_well_formed)
  |> (add_phase desugared desugar)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename_and_tag)
  |> (add_phase anfed (fun p -> atag (anf p)))
  |> (add_phase locate_bindings (pick_alloc_strategy alloc_strat))
  |> (add_phase result compile_prog)
;;
