type kind = BOOL | INT | FLOAT
type bool_t = [ `Bool of bool ]
type int_t = [ `Int of int ]
type float_t = [ `Float of float ]
type var_t = [ `Var of kind * string ]
type raw_t = [ `Raw of Sexplib.Sexp.t list ]
type atom_t =
    [ `Bool of bool
    | `Float of float
    | `Int of int
    | `Raw of Sexplib.Sexp.t list
    | `Var of kind * string ]
type expr_t =
    [ `Add of expr_t * expr_t
    | `And of expr_t * expr_t
    | `Bool of bool
    | `Div of expr_t * expr_t
    | `Eq of expr_t * expr_t
    | `Float of float
    | `Gt of expr_t * expr_t
    | `Gte of expr_t * expr_t
    | `Iff of expr_t * expr_t
    | `Implies of expr_t * expr_t
    | `Int of int
    | `Ite of expr_t * expr_t * expr_t
    | `Lt of expr_t * expr_t
    | `Lte of expr_t * expr_t
    | `Mul of expr_t * expr_t
    | `Neg of expr_t
    | `Not of expr_t
    | `Or of expr_t * expr_t
    | `Pow of expr_t * expr_t
    | `Raw of Sexplib.Sexp.t list
    | `Sub of expr_t * expr_t
    | `Var of kind * string ]
type t =
    [ `Add of expr_t * expr_t
    | `And of expr_t * expr_t
    | `Bool of bool
    | `Div of expr_t * expr_t
    | `Eq of expr_t * expr_t
    | `Float of float
    | `Gt of expr_t * expr_t
    | `Gte of expr_t * expr_t
    | `Iff of expr_t * expr_t
    | `Implies of expr_t * expr_t
    | `Int of int
    | `Ite of expr_t * expr_t * expr_t
    | `Lt of expr_t * expr_t
    | `Lte of expr_t * expr_t
    | `Mul of expr_t * expr_t
    | `Neg of expr_t
    | `Not of expr_t
    | `Or of expr_t * expr_t
    | `Pow of expr_t * expr_t
    | `Raw of Sexplib.Sexp.t list
    | `Sub of expr_t * expr_t
    | `Var of kind * string ]
type sym_t =
    [ `Add of expr_t * expr_t
    | `And of expr_t * expr_t
    | `Bool of bool
    | `Div of expr_t * expr_t
    | `Eq of expr_t * expr_t
    | `Float of float
    | `Gt of expr_t * expr_t
    | `Gte of expr_t * expr_t
    | `Iff of expr_t * expr_t
    | `Implies of expr_t * expr_t
    | `Int of int
    | `Ite of expr_t * expr_t * expr_t
    | `Lt of expr_t * expr_t
    | `Lte of expr_t * expr_t
    | `Mul of expr_t * expr_t
    | `Neg of expr_t
    | `Not of expr_t
    | `Or of expr_t * expr_t
    | `Pow of expr_t * expr_t
    | `Raw of Sexplib.Sexp.t list
    | `Sub of expr_t * expr_t
    | `Var of kind * string ]
val z3_of_t : Z3.context -> expr_t -> Z3.Expr.expr
val t_of_z3 :
  Z3.Expr.expr ->
  [> `Bool of bool
   | `Float of float
   | `Int of int
   | `Raw of Sexplib.Type.t list ]
module Infix :
  sig
    val ctx : Z3.context ref
    val z3_of_t : expr_t -> Z3.Expr.expr
    val ( && ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `And of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( || ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Or of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( + ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Add of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( - ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Sub of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( * ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Mul of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( / ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Div of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( ** ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Pow of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( > ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Gt of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( >= ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Gte of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( < ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Lt of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ( <= ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Lte of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] *
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val neg :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Neg of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val not :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      [> `Not of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] ]
    val ite :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      'a ->
      'b ->
      [> `Ite of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] * 'a * 'b ]
    val ( <==> ) :
      [ `Add of expr_t * expr_t
      | `And of expr_t * expr_t
      | `Bool of bool
      | `Div of expr_t * expr_t
      | `Eq of expr_t * expr_t
      | `Float of float
      | `Gt of expr_t * expr_t
      | `Gte of expr_t * expr_t
      | `Iff of expr_t * expr_t
      | `Implies of expr_t * expr_t
      | `Int of int
      | `Ite of expr_t * expr_t * expr_t
      | `Lt of expr_t * expr_t
      | `Lte of expr_t * expr_t
      | `Mul of expr_t * expr_t
      | `Neg of expr_t
      | `Not of expr_t
      | `Or of expr_t * expr_t
      | `Pow of expr_t * expr_t
      | `Raw of Sexplib.Sexp.t list
      | `Sub of expr_t * expr_t
      | `Var of kind * string ] ->
      'a ->
      [> `Iff of
           [ `Add of expr_t * expr_t
           | `And of expr_t * expr_t
           | `Bool of bool
           | `Div of expr_t * expr_t
           | `Eq of expr_t * expr_t
           | `Float of float
           | `Gt of expr_t * expr_t
           | `Gte of expr_t * expr_t
           | `Iff of expr_t * expr_t
           | `Implies of expr_t * expr_t
           | `Int of int
           | `Ite of expr_t * expr_t * expr_t
           | `Lt of expr_t * expr_t
           | `Lte of expr_t * expr_t
           | `Mul of expr_t * expr_t
           | `Neg of expr_t
           | `Not of expr_t
           | `Or of expr_t * expr_t
           | `Pow of expr_t * expr_t
           | `Raw of Sexplib.Sexp.t list
           | `Sub of expr_t * expr_t
           | `Var of kind * string ] * 'a ]
    val ( ==> ) : 'a -> 'b -> [> `Implies of 'a * 'b ]
    val var : 'a -> 'b -> [> `Var of 'a * 'b ]
    val int : 'a -> [> `Int of 'a ]
    val real : 'a -> [> `Float of 'a ]
    val raw : 'a -> [> `Raw of 'a ]
    val bool : 'a -> [> `Bool of 'a ]
    val solve :
      ?solver:Z3.Solver.solver ->
      expr_t list -> Z3.Solver.status * Z3.Solver.solver
  end
