(*
   Incremental linear algebra solver
   (C) 2008, Nathan Mishra-Linger
   License: BSD
*)

(* CR: parameterize this code by the underlying field of scalars *)

(* linear algebra expressions *)
type t
val var    : unit -> t (* fresh variable *)
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

