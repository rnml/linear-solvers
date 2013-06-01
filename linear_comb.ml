open Core.Std

include Int.Replace_polymorphic_compare

type scalar = float

module type Var = sig
  type t with compare
  val create : unit -> t
end

module type S = sig

  type var
  type t

  val var    : var -> t (* fresh variable *)
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

module Make (Var : Var) = struct

  type var = Var.t

  type term = Prod of scalar * Var.t
  type t = Sum of term list * scalar

  let const c = Sum ([], c)
  let var x = Sum ([Prod (1.0, x)], 0.0)

  (* zero-coefficient-avoiding cons for term list *)
  let cons (Prod (a, _) as t) ts =
    if Float.equal a 0.0 then ts else t :: ts

  (* zero-coefficient-avoiding term list map *)
  let term_map f xs =
    List.fold_right xs ~f:(fun x xs -> cons (f x) xs) ~init:[]

  let times c (Sum (ts, b)) =
    let ts = term_map (fun (Prod (a, x)) -> Prod (a *. c, x)) ts in
    Sum (ts, b *. c)

  let div (Sum (ts, b)) c =
    let ts = term_map (fun (Prod (a, x)) -> Prod (a /. c, x)) ts in
    Sum (ts, b /. c)

  let plus (Sum (ts1, b1)) (Sum (ts2, b2)) =
    (* merge two sorted lists, summing coefficients with the same
       variable *)
    let rec merge = function
      | [], ts | ts, [] -> ts
      | (Prod (a1, x1) as t1 :: ts1 as tts1),
        (Prod (a2, x2) as t2 :: ts2 as tts2) ->
        begin
          match Ordering.of_int (Var.compare x1 x2) with
          | Less    -> t1 :: merge (ts1, tts2)
          | Greater -> t2 :: merge (tts1, ts2)
          | Equal   -> cons (Prod (a1 +. a2, x1)) (merge (ts1, ts2))
        end
    in
    Sum (merge (ts1, ts2), b1 +. b2)

  let negate t = times (-1.0) t
  let minus t1 t2 = plus t1 (negate t2)
  let one = const 1.0
  let zero = const 0.0
  let sum ts = List.fold_left ~f:plus ~init:zero ts

end

