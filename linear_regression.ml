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
  }

  let compare t1 t2 = Linear.Var.compare t1.var t2.var

  let create_aux () = {
    var = Linear.Var.create ();
    esum = Linear.zero;
  }

  let all = Queue.create ()

  let create () =
    let t = create_aux () in
    Queue.enqueue all t;
    t

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
  )

let value x =
  Queue.iter Var.all ~f:(fun x ->
    try Linear.equate x.Var.esum Linear.zero
    with _ -> ());
  Linear.value (Linear.var x.Var.var)

