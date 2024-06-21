
type name = {fname: string; sname: string}

let rec merge_int_lists(ls) (ls') =
	match (ls, ls') with
	| (hd::tl, []) when hd + 0 = hd -> hd::tl
	| ([], hd::tl) -> hd::tl
	| (hd::tl, hd'::tl') -> if hd < hd' then hd::(merge_int_lists (tl) (hd'::tl'))
										else hd'::(merge_int_lists (hd::tl) (tl'))
	| _ -> [];;

let rec merge_any_lists cmp ls ls' =
	match (ls, ls') with
	| (hd::tl, []) -> hd::tl
	| ([], hd::tl) -> hd::tl
	| (hd::tl, hd'::tl') -> if cmp hd hd' > 0	then hd'::(merge_any_lists cmp (hd::tl) (tl'))
												else hd::(merge_any_lists cmp (tl) (hd'::tl'))
	| _ -> [];;

let merge_string_lists =
	merge_any_lists String.compare;;

let (>>) f g = fun x -> f (g(x));;

(* That is nice, isn't it? My head aches. It works, but this is overhead IMHO *)
(* let merge_name_lists =
	merge_any_lists (let rec coalesce ls =
						match ls with
						| x::tl when x <> 0 -> x
						| x::tl -> coalesce tl
						| _ -> 0
						in
						let uncurry f (x, y) = f x y in
						let map_pair f (x, y) = (f x, f y) in
						let cmp_names_inner (n, n') = 
							coalesce >> List.map (uncurry compare)
								>> List.map (fun f -> map_pair f (n, n')) in
						let cmp_names (n, n') =
							cmp_names_inner (n, n') [(fun nam -> nam.fname); (fun nam -> nam.sname)] in
						(fun n n' -> cmp_names (n, n'))
					);; *)

let merge_name_lists =
	merge_any_lists (fun n n' -> let r = compare n.fname n'.fname in
						if r != 0 then r else compare n.sname n'.sname);;

(* todo: partial arguments applying (complains about _ can't be generalized) *)
let len (ls) = List.fold_left (fun (acc) (_) -> acc + 1) 0 ls;;

(* let len ls = List.fold_left (+) 0 (List.map (fun _ -> 1) ls);; *)



let forall predicate ls = List.fold_left (fun result curr -> result && predicate curr) true ls;;

(* let forall predicate = List.fold_left (fun acc x -> acc && (predicate x)) true;; *)

(* let len ls = (List.fold_left (+) 0 >> List.map (fun _ -> 1)) ls;; *)

(* let forall predicate = List.fold_left (&&) true >> List.map predicate;; *)

(* let exists predicate = List.fold_left (||) false >> List.map predicate;; *)

let exists predicate ls = List.fold_left (fun result curr -> result || predicate curr) false ls;;

(* let exists predicate = List.fold_left (fun acc x -> acc || (predicate x)) false;; *)


type typ =
  | TypI (* int *)
  | TypB (* bool *)
  | TypF of typ * typ (* function: T1 -> T2 *);;


type op = Add | Mult | Eq | Less;;

type expr =
  | ConstI of int
  | ConstB of bool
  | Var of string
  | BinOp of op * expr * expr (* Operation type, first subexpression, second subexpression *)
  | Let of string * expr * expr (* var name, expression to bind, let body *)
  | Fun of string * typ * expr (* parameter name, parameter type, function body *)
  | Call of string * expr (* function name, argument expression *)
  | If of expr * expr * expr (* Text expression, then branch, else branch *);;

exception UndefinedVariable of string;;
exception TypeError;;


let rec typecheck (exp) =
	let rec lookup(name)(environment) =
	  match environment with
	  | (name', t)::tail -> if name = name' then t else lookup name tail
	  | _ -> raise (UndefinedVariable name)
	in
	let rec typecheck_env (exp) (env) =
		match exp with
		| ConstI(_) -> TypI
		| ConstB(_) -> TypB
		| Var(name) -> lookup name env
		| BinOp(op, e, e') when typecheck_env e env = TypI && typecheck_env e' env = TypI ->
							if op == Add || op == Mult
								then TypI
								else if op == Eq || op == Less then TypB
							else raise TypeError
		| Let(name, e, e') -> let t = typecheck_env e env in typecheck_env e' ((name, t)::env)
		| Fun(name, tp, e) -> let t = typecheck_env e ((name, tp)::env) in TypF(tp, t)
		| Call(name, e) -> (let tp = (typecheck_env e env) in
										match lookup name env with
													| TypF(tp', t) when tp = tp' -> t
													| _ -> raise TypeError
										)
		| If(e, e', e'') when typecheck_env e env = TypB -> let t' = typecheck_env e' env in
									let t'' = typecheck_env e'' env in
										if t' = t'' then t' else raise TypeError
		| _ -> raise TypeError
	in typecheck_env exp []