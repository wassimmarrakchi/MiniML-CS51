(*
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions
 *)

type unop =
  | Negate
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
and varid = string

(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  (* variables *)
  | Var id -> SS.add id SS.empty
  (* integers and other literals *)
  | Num _ | Bool _ | Raise | Unassigned -> SS.empty
  (* unary operators *)
  | Unop (_, exp) -> free_vars exp
  (* binary operators *)
  | Binop (_, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  (* Conditionals *)
  | Conditional (exp1, exp2, exp3) ->
    SS.union (SS.union (free_vars exp1) (free_vars exp2)) (free_vars exp3)
  (* functions *)
  | Fun (id, exp) -> SS.diff (free_vars exp) (free_vars (Var id))
  (* binding *)
  | Let (id, exp1, exp2) ->
    SS.union (SS.diff (free_vars exp2) (free_vars (Var id))) (free_vars exp1)
  (* applications *)
  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  (* recursive binding *)
  | Letrec (id, exp1, exp2) ->
    SS.diff (free_vars (Let (id, exp1, exp2))) (free_vars (Var id))
;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let ctr = ref 0 ;;
let new_varname () : varid =
  let v = "var" ^ string_of_int (!ctr) in
  ctr := !ctr + 1;
  v ;;

(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let sub = subst var_name repl in
  match exp with
  (* variables *)
  | Var id -> if id = var_name then repl else exp
  (* integers, booleans, floats, exceptions and temporarily unassigned *)
  | Num _ | Bool _ | Raise | Unassigned -> exp
  (* unary operators *)
  | Unop (op, e) -> Unop (op, sub e)
  (* binary operators *)
  | Binop (op, e1, e2) -> Binop (op, sub e1, sub e2)
  (* if then else *)
  | Conditional (e1, e2, e3) -> Conditional (sub e1, sub e2, sub e3)
  (* function definitions *)
  | Fun (id, e) ->
    if id = var_name then exp
    else if SS.mem id (free_vars repl) then
      let variable = new_varname () in
      Fun (variable, sub (subst id (Var variable) e))
    else Fun (id, sub e)
  (* local binding *)
  | Let (id, def, body) ->
    if id = var_name then Let (id, sub def, body)
    else if SS.mem id (free_vars repl) then
      let variable = new_varname () in
      Let (variable, sub def, sub (subst id (Var variable) body))
    else Let (id, sub def, sub body)
  (* function applications *)
  | App (e1, e2) -> App (sub e1, sub e2)
  (* recursive local naming *)
  | Letrec (id, def, body) ->
    if id = var_name then exp
    else if SS.mem id (free_vars repl) then
      let variable = new_varname () in
      let id_to_new = subst id (Var variable) in
      Letrec (variable, sub (id_to_new def), sub (id_to_new body))
    else Letrec (id, sub def, sub body)
;;

(*......................................................................
  String representations of expressions
 *)


(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var id -> id
  (* integers *)
  | Num n -> string_of_int n
  (* booleans *)
  | Bool b -> string_of_bool b
  (* unary operators *)
  | Unop (Negate, uexp) -> "~-(" ^ exp_to_concrete_string uexp ^ ")"
  (* binary operators *)
  | Binop (Plus, b1, b2) ->
    "(" ^ exp_to_concrete_string b1 ^ " + " ^ exp_to_concrete_string b2 ^ ")"
  | Binop (Minus, b1, b2) ->
    "(" ^ exp_to_concrete_string b1 ^ " - " ^ exp_to_concrete_string b2 ^ ")"
  | Binop (Times, b1, b2) ->
    exp_to_concrete_string b1 ^ " * " ^ exp_to_concrete_string b2
  | Binop (Equals, b1, b2) ->
    exp_to_concrete_string b1 ^ " = " ^ exp_to_concrete_string b2
  | Binop (LessThan, b1, b2) ->
    exp_to_concrete_string b1 ^ " < " ^ exp_to_concrete_string b2
  (* if then else *)
  | Conditional (cexp1, cexp2, cexp3) ->
    "if " ^ exp_to_concrete_string cexp1 ^
    " then " ^ exp_to_concrete_string cexp2 ^
    " else " ^ exp_to_concrete_string cexp3
  (* function definitions *)
  | Fun (id, expr) -> "(fun " ^ id ^ " -> " ^ exp_to_concrete_string expr ^ ")"
  (* local naming *)
  | Let (id, exp1, exp2) ->
    "let " ^ id ^ " = " ^ exp_to_concrete_string exp1 ^
    " in " ^ exp_to_concrete_string exp2
  (* recursive local naming *)
  | Letrec (id, exp1, exp2) ->
    "let rec " ^ id ^ " = " ^ exp_to_concrete_string exp1 ^
    " in " ^ exp_to_concrete_string exp2
  (* exceptions *)
  | Raise -> "EXCEPTION"
  (* (temporarily) unassigned *)
  | Unassigned -> "UNASSIGNED"
  (* function applications *)
  | App (e1, e2) ->
    exp_to_concrete_string e1 ^ "(" ^ exp_to_concrete_string e2 ^ ")"
;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var id -> "Var " ^ id
  (* integers *)
  | Num n -> "Num " ^ string_of_int n
  (* booleans *)
  | Bool b -> "Bool " ^ string_of_bool b
  (* unary operators *)
  | Unop (Negate, uexp) -> "Unop(Negate, " ^ exp_to_abstract_string uexp ^ ")"
  (* binary operators *)
  | Binop (Plus, b1, b2) ->
    "Binop(Plus, " ^ exp_to_abstract_string b1 ^ ", "
                   ^ exp_to_abstract_string b2 ^ ")"
  | Binop (Minus, b1, b2) ->
    "Binop(Minus, " ^ exp_to_abstract_string b1 ^ ", "
                      ^ exp_to_abstract_string b2 ^ ")"
  | Binop (Times, b1, b2) ->
    "Binop(Times, " ^ exp_to_abstract_string b1 ^ ", "
                    ^ exp_to_abstract_string b2 ^ ")"
  | Binop (Equals, b1, b2) ->
    "Binop(Equals, " ^ exp_to_abstract_string b1 ^ ", "
                     ^ exp_to_abstract_string b2 ^ ")"
  | Binop (LessThan, b1, b2) ->
    "Binop(LessThan, " ^ exp_to_abstract_string b1 ^ ", "
                       ^ exp_to_abstract_string b2 ^ ")"
  (* if then else *)
  | Conditional (cexp1, cexp2, cexp3) ->
    "Conditional(" ^ exp_to_abstract_string cexp1 ^ ", "
                   ^ exp_to_abstract_string cexp2 ^ ", "
                   ^ exp_to_abstract_string cexp3 ^ ")"
  (* function definitions *)
  | Fun (id, expr) -> "Fun(" ^ id ^ ", " ^ exp_to_abstract_string expr ^ ")"
  (* local naming *)
  | Let (id, exp1, exp2) ->
    "Let(" ^ id ^ ", " ^ exp_to_abstract_string exp1 ^
                  ", " ^ exp_to_abstract_string exp2 ^ ")"
  (* recursive local naming *)
  | Letrec (id, exp1, exp2) ->
    "Letrec(" ^ id ^ ", " ^ exp_to_abstract_string exp1 ^
                     ", " ^ exp_to_abstract_string exp2 ^ ")"
  (* exceptions *)
  | Raise -> "EXCEPTION"
  (* (temporarily) unassigned *)
  | Unassigned -> "UNASSIGNED"
  (* function applications *)
  | App (e1, e2) ->
    "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
;;
