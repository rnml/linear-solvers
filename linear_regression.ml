(** Incremental linear regression solver *)
(*
   Copyright 2013, Nathan Mishra-Linger
   License: BSD
*)

(* the implementation is built on top of the incremental linear
   arithmetic solver in linear.ml *)

open Core.Std

include Int.Replace_polymorphic_compare

module Var = struct
  type t = {
    var : Linear.Var.t;
    mutable esum : Linear.t;
    related : t list Union_find.t (* sorted list *)
  }

  let compare t1 t2 = Linear.Var.compare t1.var t2.var

  let create () =
    let t = {
      var = Linear.Var.create ();
      esum = Linear.zero;
      related = Union_find.create [];
    } in
    Union_find.set t.related [t];
    t

  let rec merge = function
    | ([], ts) | (ts, []) -> ts
    | ((t1 :: ts1 as tts1), (t2 :: ts2 as tts2)) ->
      match Ordering.of_int (compare t1 t2) with
      | Equal   -> t1 :: merge (ts1, ts2)
      | Less    -> t1 :: merge (ts1, tts2)
      | Greater -> t2 :: merge (tts1, ts2)

  let relate t1 t2 =
    let r1 = t1.related in
    let r2 = t2.related in
    if not (Union_find.same_class r1 r2) then begin
      let ts = merge (Union_find.get r1, Union_find.get r2) in
      Union_find.union r1 r2;
      Union_find.set r1 ts;
    end

end

include Linear_comb.Make (Var)

module Coerce = struct
  let var x = x.Var.var
  let term (Prod (c, x)) = Linear.times c (Linear.var (var x))
  let sum ts b = Linear.sum (Linear.const b :: List.map ~f:term ts)
end

let equate t1 t2 =
  let Sum (ts, b) = minus t1 t2 in
  (* Sum ts + b ~= 0 *)
  (* Sum ts + b = e *)
  let e = Linear.fresh () in
  Linear.equate e (Coerce.sum ts b);
  List.iter ts ~f:(fun (Prod (c, x)) ->
    x.Var.esum <- Linear.plus x.Var.esum (Linear.times c e)
  );
  match ts with
  | [] -> ()
  | Prod (_, x) :: ts ->
    List.iter ts ~f:(fun (Prod (_, x')) -> Var.relate x x')

let value x =
  List.iter (Union_find.get x.Var.related) ~f:(fun x ->
    try Linear.equate x.Var.esum Linear.zero
    with _ -> ());
  Linear.value (Linear.var x.Var.var)

