open Core.Std
open Solvers.Std

module Var : Identifiable

module Env : sig
  type t with sexp_of
  val create : unit -> t
end

module Expr : sig
  type t =
  | Var    of Var.t
  | Const  of float
  | Times  of t * t
  | Div    of t * t
  | Plus   of t * t
  | Minus  of t * t
  | Negate of t
  with sexp_of

  val eval : Env.t -> t -> Linear.t
end

module Stmt : sig
  type t = Equate of Expr.t * Expr.t * Expr.t list with sexp_of

  val eval : Env.t -> t -> unit
end
