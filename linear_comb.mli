(* linear combinations *)
open Core.Std

type scalar = float

module type Var = sig
  type t with compare
  val create : unit -> t
end

module type S = sig

  type var
  type t

  val var    : var-> t
  val fresh  : unit -> t (* fresh variable *)
  val const  : scalar -> t
  val times  : scalar -> t -> t
  val div    : t -> scalar -> t
  val plus   : t -> t -> t
  val minus  : t -> t -> t
  val negate : t -> t
  val one    : t
  val zero   : t
  val sum    : t list -> t

end

module type S_concrete = sig
  (* INVARIANTS:
     (1) no term coefficient is zero
     (2) term list in Sum is sorted by variable
  *)
  type var
  type term = Prod of scalar * var
  type t = Sum of term list * scalar
  include S with type t := t and type var := var
end

module Make (Var : Var) : S_concrete with type var = Var.t
