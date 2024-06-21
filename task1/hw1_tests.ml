open Hw1;;

(* is_positive: int list -> bool *)
assert(is_positive [] = true);;
assert(is_positive [4] = true);;
assert(is_positive [4; -1] = false);;


(* is_sorted: int list -> bool *)
assert(is_sorted [] = true);;
assert(is_sorted [4] = true);;
assert(is_sorted [4; -1] = false);;
assert(is_sorted [1; 2; 3; 4] = true);;
assert(is_sorted [1; 1; 1; 1] = true);;
assert(is_sorted [1; 2; 4; 3] = false);;

let tree = Br(4, Br(1, Lf, Br(3, Br(2, Lf, Lf), Lf)), Br(5, Lf, Br(6, Lf, Lf)));;
let tree3 = Br(3, Lf, Br(2, Lf, Br(1, Lf, Lf)));;
 
(*count_branches: int_tree -> int *)
assert(count_branches Lf = 0);;
assert(count_branches (Br(1, Lf, Lf)) = 1);;
assert(count_branches tree = 6);;
assert(count_branches tree3 = 3);;

(* depth: int_tree -> int *)
assert(depth Lf = 0);;
assert(depth (Br(1, Lf, Lf)) = 1);;
assert(depth tree3 = 3);;
assert(depth tree = 4);;

(* gen_tree: int -> int_tree *)
assert(gen_tree 0 = Lf);;
assert(gen_tree 1 = Br(1, Lf, Lf));;
assert(gen_tree 3 = tree3);

(* inorder: int_tree -> int list *)
assert(inorder(Br(1, Lf, Lf))  = [1]);;
assert(inorder tree = [1; 2; 3; 4; 5; 6]);;

(* preorder: int_tree -> int list *)
assert(preorder(Br(1, Lf, Lf))  = [1]);;
assert(preorder tree = [4; 1; 3; 2; 5; 6]);;

let library = [
    {
        author = {fname = "Niccolo"; sname = "Machiavelli"};
        name = "The Prince";
        year = 1513
    };
    {
        author = {fname = "Thomas"; sname = "Hobbes"};
        name = "Leviathan";
        year = 1651
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Theory of Moral Sentiments";
        year = 1759
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Wealth of Nations";
        year = 1776
    };
    {
        author = {fname = "John"; sname = "Smith"};
        name = "Odes Paraphras'd and imitated in Miscellany Poems and Translations by Oxford Hands";
        year = 1685
    }
];;

(* search_book: key -> book_type list -> book_type list *)
assert(search_book "hello" [] = []);;
assert(search_book "Smith" library = [
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Theory of Moral Sentiments";
        year = 1759
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Wealth of Nations";
        year = 1776
    };
    {
        author = {fname = "John"; sname = "Smith"};
        name = "Odes Paraphras'd and imitated in Miscellany Poems and Translations by Oxford Hands";
        year = 1685
    }
]);;
assert(search_book "Adam" library = [
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Theory of Moral Sentiments";
        year = 1759
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Wealth of Nations";
        year = 1776
    }
]);;
assert(search_book "The" library = [
    {
        author = {fname = "Niccolo"; sname = "Machiavelli"};
        name = "The Prince";
        year = 1513
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Theory of Moral Sentiments";
        year = 1759
    };
    {
        author = {fname = "Adam"; sname = "Smith"};
        name = "The Wealth of Nations";
        year = 1776
    }
]);;
assert(search_book "Prince" library = [
    {
        author = {fname = "Niccolo"; sname = "Machiavelli"};
        name = "The Prince";
        year = 1513
    }
]);;
assert(search_book "Leviathan" library = [
    {
        author = {fname = "Thomas"; sname = "Hobbes"};
        name = "Leviathan";
        year = 1651
    }
]);;

(* simplify: bool_expr - > bool_expr *)
assert(simplify(Const(True)) = Const(True));;
assert(simplify(And(Const(True), Const(False))) = Const(False));;
assert(simplify(
    Or(
        Const(False),
        Or(
            Const(False),
            Or(
                And(
                    Const(True),
                    Const(False)
                ),
                Const(True)
            )
        )
    )
) = Const(True));;
assert(simplify(And(Var("any_var"), Const(True))) = Var("any_var"));;

(* lookup: string -> (string * letlang_expr) list -> letlang_expr option *)

assert((lookup "x" []) = None);;
assert((lookup "x" [("x", Const 15)]) = Some (Const 15));;
assert((lookup "x" [("x", Const 15); ("x", Const 14)]) = Some (Const 15));;

(* eval: letlang_expr -> letlang_expr *)

let e1 = Const 42;;
let e2 = Var "x";;
let e3 = Let("y", Const 15, Var "y");;
let e4 = Let("y", Const 15, Let("y", Const 14, Var "y"));;

assert((eval e1) = (Const 42));;
assert(try ignore((eval e2)); false with
        | UndefinedVariable v -> (v = "x")
        | _ -> false);;

assert((eval e3) = Const(15));;
assert((eval e4) = Const(14));;

Printf.printf "All Tests passed!\n";;
