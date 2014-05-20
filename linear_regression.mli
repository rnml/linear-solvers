(*
   Incremental linear regression solver
   (C) 2013, Nathan Mishra-Linger
   License: BSD
*)

module Var : sig
  type t with compare
  val create : unit -> t
end

(* linear algebra expressions *)
type t
val var    : Var.t -> t
val fresh  : unit -> t
val const  : float -> t
val times  : float -> t -> t
val div    : t -> float -> t
val plus   : t -> t -> t
val minus  : t -> t -> t
val negate : t -> t
val one    : t
val zero   : t
val sum    : t list -> t

(* solver: assert equations and determine expression values *)

val equate : t -> t -> unit

val value : Var.t -> float option
