open Z3

type kind =
    | BOOL
    | INT
    | FLOAT

type bool_t = [`Bool of bool]
type int_t = [`Int of int]
type float_t = [`Float of float]
type var_t = [`Var of kind * string]
type raw_t = [`Raw of Sexplib.Sexp.t list]

type atom_t = [
    | bool_t
    | int_t
    | float_t
    | var_t
    | raw_t
]

type expr_t = [
    | atom_t
    | `And of expr_t * expr_t
    | `Or of expr_t * expr_t
    | `Add of expr_t * expr_t
    | `Sub of expr_t * expr_t
    | `Mul of expr_t * expr_t
    | `Div of expr_t * expr_t
    | `Pow of expr_t * expr_t
    | `Gt of expr_t * expr_t
    | `Lt of expr_t * expr_t
    | `Gte of expr_t * expr_t
    | `Lte of expr_t * expr_t
    | `Eq of expr_t * expr_t
    | `Not of expr_t
    | `Neg of expr_t
    | `Ite of expr_t * expr_t * expr_t
    | `Iff of expr_t * expr_t
    | `Implies of expr_t * expr_t
]

type t = [
    | atom_t
    | expr_t
]

type sym_t = [
    | var_t
    | raw_t
    | expr_t
]

let rec z3_of_t ctx : t -> Z3.Expr.expr = function
    | `Bool true -> Boolean.mk_true ctx
    | `Bool false -> Boolean.mk_false ctx
    | `Int i -> Arithmetic.Integer.mk_numeral_i ctx i
    | `Float f -> Arithmetic.Real.mk_numeral_s ctx (string_of_float f)
    | `Var (BOOL, name) -> Boolean.mk_const_s ctx name
    | `Var (INT, name) -> Arithmetic.Integer.mk_const_s ctx name
    | `Var (FLOAT, name) -> Arithmetic.Real.mk_const_s ctx name
    | `Raw s ->
        let s = List.map Sexplib.Sexp.to_string s in
        let s = String.concat "\n" s in
        SMT.parse_smtlib2_string ctx s [] [] [] []
    | `And (a, b) -> Boolean.mk_and ctx [z3_of_t ctx a; z3_of_t ctx b]
    | `Or (a, b) -> Boolean.mk_or ctx [z3_of_t ctx a; z3_of_t ctx b]
    | `Add (a, b) -> Arithmetic.mk_add ctx [z3_of_t ctx a; z3_of_t ctx b]
    | `Sub (a, b) -> Arithmetic.mk_sub ctx [z3_of_t ctx a; z3_of_t ctx b]
    | `Mul (a, b) -> Arithmetic.mk_mul ctx [z3_of_t ctx a; z3_of_t ctx b]
    | `Div (a, b) -> Arithmetic.mk_div ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Pow (a, b) -> Arithmetic.mk_power ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Gt (a, b) -> Arithmetic.mk_gt ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Lt (a, b) -> Arithmetic.mk_lt ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Gte (a, b) -> Arithmetic.mk_ge ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Lte (a, b) -> Arithmetic.mk_le ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Eq (a, b) ->
        let a = z3_of_t ctx a in
        let b = z3_of_t ctx b in
        let le = Arithmetic.mk_le ctx a b in
        let ge = Arithmetic.mk_ge ctx a b in
        Boolean.mk_and ctx [le; ge]
    | `Not a -> Boolean.mk_not ctx (z3_of_t ctx a)
    | `Neg a -> Arithmetic.mk_unary_minus ctx (z3_of_t ctx a)
    | `Ite (a, b, c) -> Boolean.mk_ite ctx (z3_of_t ctx a) (z3_of_t ctx b) (z3_of_t ctx c)
    | `Iff (a, b) -> Boolean.mk_iff ctx (z3_of_t ctx a) (z3_of_t ctx b)
    | `Implies (a, b) -> Boolean.mk_implies ctx (z3_of_t ctx a) (z3_of_t ctx b)

let t_of_z3 = function
    | x when Boolean.is_bool x -> `Bool (Boolean.is_true x)
    | x when Arithmetic.is_int x -> `Int (Arithmetic.Integer.get_int x)
    | x when Arithmetic.is_real x -> `Float (Arithmetic.Real.get_ratio x |> Ratio.float_of_ratio)
    | x  ->
        let s = Z3.Expr.to_string x in
        `Raw (Sexplib.Parser.sexps Sexplib.Lexer.main (Lexing.from_string s))

module Infix = struct

    let ctx = ref (mk_context ["proof", "true"; "model", "true"])

    let z3_of_t x =
        z3_of_t !ctx x

    let ( && ) (a : [bool_t|sym_t]) (b : [bool_t|sym_t]) =
        `And (a, b)

    let ( || ) (a : [bool_t|sym_t]) (b : [bool_t|sym_t]) =
        `Or (a, b)

    let ( + ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Add (a, b)

    let ( - ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Sub (a, b)

    let ( * ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Mul (a, b)

    let ( / ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Div (a, b)

    let ( ** ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Pow (a, b)

    let ( > ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Gt (a, b)

    let ( >= ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Gte (a, b)

    let ( < ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Lt (a, b)

    let ( <= ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Lte (a, b)

    let ( == ) (a : [int_t|float_t|sym_t]) (b : [int_t|float_t|sym_t]) =
        `Eq (a, b)

    let neg (a : [int_t|float_t|sym_t]) =
        `Neg a

    let not (a : [bool_t|sym_t]) =
        `Not a

    let ite (a : [bool_t|sym_t]) b c =
        `Ite (a, b, c)

    let ( <==> ) (a : [bool_t|sym_t]) b =
        `Iff (a, b)

    let ( ==> ) a b =
        `Implies (a, b)

    let var a b = `Var (a, b)
    let int i = `Int i
    let real f = `Float f
    let raw l = `Raw l
    let bool b = `Bool b
    let solve ?solver l =
        let s = match solver with
            None -> Solver.mk_simple_solver !ctx | Some s -> s in
        Solver.add s (List.map z3_of_t l);
        Solver.check s [], s

    let get_model solver =
        Solver.get_model solver

    let eval model expr =
        Z3.Model.eval model (z3_of_t expr) true

end

