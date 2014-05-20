(*
   Incremental linear algebra solver
   (C) 2008, Nathan Mishra-Linger
   License: BSD
*)

(* CR: parameterize this code by the underlying field of scalars *)

module Var : sig
  type t with compare
  val create : unit -> t
end

(* linear algebra expressions *)
type t
val var    : Var.t -> t
val fresh  : unit -> t (* var (Var.create ()) *)
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
  (* throws *)
  exception Inconsistent
    (* if equation is inconsistent w.r.t. previously asserted equations *)
  exception Redundant
    (* if equation is redundant w.r.t. previously asserted equations *)

val value : t -> float option
