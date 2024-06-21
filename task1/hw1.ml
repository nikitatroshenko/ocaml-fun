let rec is_positive(l) = 
  match l with
  | [] -> true
  | head::tail -> head > 0 && is_positive tail;;

let rec is_sorted(l) =
  match l with
  | [] -> true
  | a::[] -> true
  | a::b::tail -> a <= b && is_sorted (b::tail);;

(* --------------- Trees -------------- *)

type int_tree = Lf | Br of int * int_tree * int_tree

let rec count_branches(tree) =
  match tree with
  | Lf -> 0
  | Br(a, l, r) -> 1 + count_branches l + count_branches r;;

let rec depth(tree) =
  match tree with
  | Lf -> 0
  | Br(a, l, r) -> 1 + max (depth l) (depth r);;

let rec gen_tree(n) =
  if n = 0
    then Lf
    else Br(n, Lf, gen_tree (n-1));;

let rec inorder(tree) =
  match tree with
  | Lf -> []
  | Br(a, l, r) -> (inorder l) @ (a::(inorder r));;

let rec preorder(tree) =
  match tree with
  | Lf -> []
  | Br(a, l, r) -> a :: (preorder l) @ (preorder r);;

(* ============== Books =============== *)

type author_type = {fname: string; sname: string}
type book_type = {author: author_type; name: string; year: int}

let rec search_book(key)(books) =
  let is_match(key)(book) =
    book.author.fname = key || book.author.sname = key ||
    let rec contains_key(key)(l) =
      match l with
      | [] -> false
      | head::tail -> key = head || contains_key key tail
    in
      contains_key key (String.split_on_char ' ' book.name)
  in
  match books with
  | [] -> []
  | book::others -> if is_match key book
                      then book::(search_book key others)
                      else search_book key others;;

(* =========== boolean ============ *)

type const = True | False
type bool_expr =
 | Const of const
 | Var of string
 | Not of bool_expr
 | And of bool_expr * bool_expr
 | Or of bool_expr * bool_expr

let rec simplify(expr) =
  match expr with
  | And(Const(False), _) -> Const(False)
  | And(_, Const(False)) -> Const(False)
  | Or(Const(True), _) -> Const(True)
  | Or(_, Const(True)) -> Const(True)
  | And(Const(True), expr') -> simplify expr'
  | And(expr', Const(True)) -> simplify expr'
  | Or(Const(False), expr') -> simplify expr'
  | Or(expr', Const(False)) -> simplify expr'
  (* | And(expr1, expr2) -> simplify (And((simplify expr1), (simplify expr2)))
  | Or(expr1, expr2) -> simplify (Or((simplify expr1), (simplify expr2))) *)
  | _ -> expr;;

(* =========== letlang ========= *)

exception UndefinedVariable of string

type letlang_expr = 
  Const of int
  | Var of string
  | Let of string * letlang_expr * letlang_expr

let rec lookup(name)(environment) =
  match environment with
  | (name', value)::tail -> if name' = name then Some(value) else lookup name tail
  | _ -> None;;

let rec eval(expr) =
  let rec eval_env(expr)(env) =
    match expr with
    | Const(a) -> Const(a)
    | Var(v) -> (match lookup v env with | Some(value) -> value | None -> raise(UndefinedVariable v))
    | Let(n, expr1, expr2) -> eval_env expr2 ((n, eval_env(expr1) env)::env)
  in eval_env expr [];;