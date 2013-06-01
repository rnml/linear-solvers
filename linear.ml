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

include struct (*  terms of the form a1*x1 + ... + aN*xN + b  *)

  (* INVARIANTS:
     (1) no term coefficient is zero
     (2) term list in Sum is sorted by variable
  *)

  type scalar = float

  type t = Sum of term list * scalar

  and term = Mult of scalar * var

  and var = {
    tag : int; (* for var comparison *)
    mutable value : t option;
  }

end

let count = ref 0 (* min_int *)
let fresh () = (incr count; { tag = !count; value = None; })

let const c = Sum ([], c)
let var () = let x = fresh () in Sum ([Mult (1.0, x)], 0.0)

(* zero-coefficient-avoiding cons for term list *)
let cons (Mult (a, _) as t) ts =
  if Float.equal a 0.0 then ts else t :: ts

(* zero-coefficient-avoiding term list map *)
let term_map f xs =
  List.fold_right xs ~f:(fun x xs -> cons (f x) xs) ~init:[]

let times c (Sum (ts, b)) =
  let ts = term_map (fun (Mult (a, x)) -> Mult (a *. c, x)) ts in
  Sum (ts, b *. c)

let div (Sum (ts, b)) c =
  let ts = term_map (fun (Mult (a, x)) -> Mult (a /. c, x)) ts in
  Sum (ts, b /. c)

let plus (Sum (ts1, b1)) (Sum (ts2, b2)) =
  (* merge two sorted lists, summing coefficients with the same
     variable *)
  let rec merge = function
    | [], ts | ts, [] -> ts
    | (Mult (a1, x1) as t1 :: ts1 as tts1),
      (Mult (a2, x2) as t2 :: ts2 as tts2) ->
      begin
        match Ordering.of_int (Int.compare x1.tag x2.tag) with
        | Less    -> t1 :: merge (ts1, tts2)
        | Greater -> t2 :: merge (tts1, ts2)
        | Equal   -> cons (Mult (a1 +. a2, x1)) (merge (ts1, ts2))
      end
  in
  Sum (merge (ts1, ts2), b1 +. b2)

let negate t = times (-1.0) t
let minus t1 t2 = plus t1 (negate t2)
let one = const 1.0
let zero = const 0.0
let sum ts = List.fold_left ~f:plus ~init:zero ts

(* a solved variable is one that has been set equal to a term *)
let solved x = match x.value with
  | None -> false
  | Some _ -> true

(* recursively substitute for all solved variables
   until only unsolved variables remain *)
let rec subst (Sum (terms, b)) =
  let ts, terms =
    List.partition_tf terms ~f:(fun (Mult (_, x)) -> solved x)
  in
  let ts =
    List.map ts ~f:(fun (Mult (a, x)) ->
      match x.value with
      | Some t -> (let t = subst t in x.value <- Some t; times a t)
      | None -> assert false)
  in
  List.fold_left ~f:plus ~init:(Sum (terms, b)) ts

exception Inconsistent
exception Redundant

(* for numerical stability: *)
let best_coeff (Mult (a1, _) as t1) (Mult (a2, _) as t2) =
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
    let Mult (a, x) = List.fold_left ~f:best_coeff ~init:hd tl in
    (* solve for x *)
    let ts =
      List.filter ts ~f:(fun (Mult (_, y)) ->
        not (Int.equal x.tag y.tag))
    in
    let t' = Sum (ts, b) in
      x.value <- Some (div (negate t') a)

let value t = match subst t with
  | Sum ([], c) -> Some c (* no unsolved variables *)
  | Sum (_, _) -> None

