open Hw2;;

(* merge_int_lists: int list -> int list -> int list *)
assert((merge_int_lists [1; 2; 3] []) = [1; 2; 3]);;
assert((merge_int_lists [] [1; 2; 3]) = [1; 2; 3]);;
assert((merge_int_lists [1; 3] [2]) = [1; 2; 3]);;
assert((merge_int_lists [2; 4] [0; 2; 3; 5]) = [0; 2; 2; 3; 4; 5]);;
assert((merge_int_lists [0; 2; 3; 5] [2; 4]) = [0; 2; 2; 3; 4; 5]);;


(* merge_string_lists *)
assert((merge_string_lists ["a"; "c"] ["b"]) = ["a"; "b"; "c"]);;
assert((merge_string_lists ["abc"; "cab"] ["acb"; "cba"]) = ["abc"; "acb"; "cab"; "cba"]);;

(* merge_name_lists *)
(* NOTA BENE:
	since design spec says that merge accepts sorted lists I suppose
	that cmp name1 name3 = -1.
	DS also says that names should be compared by last names first
	cmp name1.sname name3.sname = 1 whilst cmp name1.fname name3.fname = -1
	so I suppose that fname is the last name (familiya) and sname is first name (not surname, ???).
	Think about it...
*)
let name1 = {fname="A"; sname="B"};;
let name2 = {fname="A"; sname="C"};;
let name3 = {fname="B"; sname="A"};;
let name4 = {fname="B"; sname="C"};;

assert((merge_name_lists [name1; name3] [name2] ) = [name1; name2; name3]);;
assert((merge_name_lists [name1; name3] [name2; name4] ) = [name1; name2; name3; name4]);;

(* len : 'a list -> int *)
assert( len [] = 0 );;
assert( len [1] = 1 );;
assert( len [1;2] = 2 );;

(* forall : ('a -> bool) -> 'a list -> bool *)

assert( (forall (fun x -> x > 0) [-1]) = false);;
assert( (forall (fun x -> x > 0) [1]) = true);;
assert( (forall (fun x -> compare x "a" >= 0) ["abc"; "a"]) = true);;
assert( (forall (fun x -> compare "a" x >= 0) ["abc"; "ba"]) = false);;
assert( (forall (fun x -> compare "b" x < 0) ["cad"; "cba"]) = true);;
assert( (forall (fun x -> compare x "b" < 0) ["abc"; "cba"]) = false);;
assert( (forall (fun x -> x = 0) []) = true);;

(* exists : ('a -> bool) -> 'a list -> bool *)

assert( (exists (fun x -> x = 0) []) = false);;
assert( (exists (fun x -> x > 0) [-1]) = false);;
assert( (exists (fun x -> x > 0) [1; -1]) = true);;
assert( (exists (fun x -> compare x "a" < 0) ["bc"; "ba"]) = false);;
assert( (exists (fun x -> compare "a" x < 0) ["babc"; "aba"]) = true);;
assert( (exists (fun x -> compare x "b" < 0) ["bc"; "ba"]) = false);;
assert( (exists (fun x -> compare "b" x < 0) ["bc"; "cba"]) = true);;

(* typecheck : expr -> Typ *)

let e1 = BinOp(Add, ConstI 42, ConstI 43);;
let e2 = Let("x", ConstI 42, Var "x");;
let e3 = Let("foo",
            Fun("x", TypI, BinOp(Mult, Var("x"), Var("x"))),
            Call("foo", ConstI 3));;
let e4 = BinOp(Mult, ConstI 42, Var "x");;
let e5 = If(ConstB true, ConstB true, ConstB false);;
let e6 = If(ConstI 42, ConstB true, ConstB false);;
let e7 = If(ConstB true, ConstI 1, ConstB false);;
let e8 = Let("foo",
            Fun("x", TypB, BinOp(Mult, Var("x"), Var("x"))),
            Call("foo", ConstB true));;
let e9 = Let("bar",
			Fun("x", TypI, If(BinOp(Less, Var("x"), ConstI 0), ConstI 0, Var("x"))),
			Call("bar", Let("baz", Fun("y", TypI, BinOp(Add, Var("y"), ConstI 1)), 
					Call("bar", ConstI 0)))
		);;
let e10 = Let("foo",
            Fun("x", TypI, BinOp(Mult, Var("x"), Var("x"))),
            Call("foo", ConstB true));;
let e11 = Let("bar",
            Fun("x", TypI, If(BinOp(Less, Var("x"), ConstI 0), ConstI 0, Call("bar", Var("x")))),
            Call("bar", Let("baz", Fun("y", TypI, BinOp(Add, Var("y"), ConstI 1)), 
                    Call("bar", ConstI 0))));;

assert( typecheck(e1) = TypI );;
assert( typecheck(e2) = TypI );;
assert( typecheck(e3) = TypI );;

assert(try ignore((typecheck e4)); false with
        | UndefinedVariable v -> (v = "x")
        | _ -> false);;

assert( typecheck(e5) = TypB );;

assert(try ignore((typecheck e6)); false with
        | TypeError -> true
        | _ -> false);;

assert(try ignore((typecheck e7)); false with
        | TypeError -> true
        | _ -> false);;

assert(try ignore((typecheck e8)); false with
        | TypeError -> true
        | _ -> false);;

assert( typecheck(e9) = TypI);;

assert(try ignore((typecheck e10)); false with
        | TypeError -> true
        | _ -> false);;

assert(try ignore((typecheck e11)); false with
        | UndefinedVariable v -> (v = "bar")
        | _ -> false);;