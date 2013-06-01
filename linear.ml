(** Incremental linear algebra solver *)
(*
   Copyright 2008, Nathan Mishra-Linger
   License: BSD
*)

(* The implementation is inspired by incremental unification as
   found in modern type inference implementations.  The classic
   reference is "Basic Polymorphic Typechecking" by Luca Cardelli.
   The linear expression type `t' is a term representation of a
   linear combination of distinct variables plus a constant.

                      a1*x1 + a2*x2 + ... + aN*xN + b

   As in efficient unification algorithms, a variable is represented
   by a possibly-null pointer to another term.  For unsolved
   variables, this pointer is null.  For solved variables, it points
   to the term to which the variable has been set equal.

   The "occurs check" of ordinary first order term unification is
   not necessary because we can always isolate the variable in
   question. (see the code for `equate', which might also be called
   `unify')
*)

open Core.Std

include Int.Replace_polymorphic_compare

module Uid = Unique_id.Int (struct end)

module rec Var : sig
  type t = {
    uid : Uid.t;
    mutable value : Comb.t option;
  }
  include Linear_comb.Var with type t := t
end = struct
  type t = {
    uid : Uid.t;
    mutable value : Comb.t option;
  }
  let compare t1 t2 = Uid.compare t1.uid t2.uid
  let create () = {uid = Uid.create (); value = None}
end

and Comb : Linear_comb.S_concrete with type var = Var.t
  = Linear_comb.Make (Var)

include Comb

let fresh () = var (Var.create ())

(* a solved variable is one that has been set equal to a term *)
let solved x =
  match x.Var.value with
  | None -> false
  | Some _ -> true

(* recursively substitute for all solved variables
   until only unsolved variables remain *)
let rec subst (Sum (terms, b)) =
  let ts, terms =
    List.partition_tf terms ~f:(fun (Prod (_, x)) -> solved x)
  in
  let ts =
    List.map ts ~f:(fun (Prod (a, x)) ->
      match x.Var.value with
      | Some t ->
        let t = subst t in
        x.Var.value <- Some t;
        times a t
      | None -> assert false)
  in
  List.fold_left ~f:plus ~init:(Sum (terms, b)) ts

exception Inconsistent
exception Redundant

(* for numerical stability: *)
let best_coeff (Prod (a1, _) as t1) (Prod (a2, _) as t2) =
  if Float.(abs a1 >= abs a2) then t1 else t2

let equate t1 t2 =
  (* it is simpler to work with the equation t = 0
     where t = t1 - t2 *)
  let t = minus t1 t2 in
  let Sum (ts, b) = subst t in
  (* check degenerate cases *)
  match ts with
  | [] ->
    raise (if Float.equal b 0.0 then Redundant else Inconsistent)
  | hd :: tl ->
    (* choose a "pivot" *)
    let Prod (a, x) = List.fold_left ~f:best_coeff ~init:hd tl in
    (* solve for x *)
    let ts =
      List.filter ts ~f:(fun (Prod (_, y)) ->
        not (Uid.equal x.Var.uid y.Var.uid))
    in
    let t' = Sum (ts, b) in
      x.Var.value <- Some (div (negate t') a)

let value t = match subst t with
  | Sum ([], c) -> Some c (* no unsolved variables *)
  | Sum (_, _) -> None

