open Core.Std

module Id : sig
  type t with sexp
  val create : unit -> t
  include Comparable.S with type t := t
end = struct
  include Int
  let count = ref Int.min_value
  let create () = (incr count; !count)
end

(* We represent a conjunction of difference logic constraints by a
   directed graph where vertices correspond to natural number valued
   variables and an edge from [a] to [b] with weight [k] means that
   [a + k <= b] or, equivalently, [k <= b - a].  Therefore, larger
   [k] values mean stronger constraints.

   This representation has some nice properties.  First, the weight
   of a path means the same thing as the weight of a single edge.

               k1      k2                    k1 <= b - a
            a ----> b ----> c            + ( k2 <= c - b )
                                        ==================
                k1 + k2                 k1 + k2 <= c - a
             a ---------> c

  A cycle in the graph with weight [k] means that k <= a - a = 0.
  If [k] is positive, then this means that the constraints along the
  cycle are contradictory. *)

type vertex = var

and var = {
  id : Id.t;
  mutable lower_bound : int; (* Always >= 0 *)
  mutable outgoing : edge Id.Map.t;
}

and edge = {
  mutable weight : int;
  target : vertex;
} with sexp

module T = struct
  module T1 = struct
    type t = var with sexp
    let equal t1 t2 = Id.equal t1.id t2.id
    let compare t1 t2 = Id.compare t1.id t2.id
  end
  include T1
  include Comparable.Make (T1)
end

let lower_bound t = t.lower_bound

let var () = {
  id = Id.create ();
  lower_bound = 0;
  outgoing = Id.Map.empty;
}

exception Inconsistent

(* invariant: v >= v' + k *)
let rec test_edge ?(visited = Id.Set.empty) v v' k' =
  if v.lower_bound < v'.lower_bound + k' then begin
    v.lower_bound <- v'.lower_bound + k';
    let visited = Id.Set.add visited v'.id in
    propagate visited v;
  end

and propagate visited v =
  if Id.Set.mem visited v.id then raise Inconsistent; (*[B]*)
  Id.Map.iter v.outgoing ~f:(fun ~key:_ ~data:e ->
    test_edge ~visited e.target v e.weight)

(* explanations for the two [Inconsistent] exceptions raised above:

  [A]

    The pseudo-variable [zero]'s lower bound should never be
    increased since it is really a constant.

  [B]

    At this point, we've traversed a cycle by increasing the
    lower_bound field of each vertex along the way.  This is only
    possible if there is a cycle in the graph corresponding to the
    following inqualities:

       v1 - v2 >= k1
       v2 - v3 >= k2
           ...
       vn - v1 >= kn

    In this cycle, [v1.lower_bound] has been updated twice, and its
    current lower_bound is now

        v1.lower_bound
      = v2.lower_bound + k1
      = v3.lower_bound + k2 + k1
      = ...
      = v1.lower_bound + kn + ... + k1

    where the final v1.lower_bound is the original value at v1.
    Since v1.lower_bound is updated only with increments, this means
    that

      kn + ... + k1 > 0

    However the inequalities in the cycle sum to yield

      0 = v1 - v1 >= k1 + ... + kn

    Therefore we've hit a contradiction.
*)

let set_geq v (`Plus (v', k')) = (* v >= v' + k' *)
  begin match Id.Map.find v'.outgoing v.id with
  | Some e ->
      if k' > e.weight then begin
        e.weight <- k';
        test_edge v v' k';
      end
  | None ->
      let e = { weight = k'; target = v } in
      v'.outgoing <- Id.Map.add v'.outgoing ~key:v.id ~data:e;
      test_edge v v' k';
  end

let set_geq a e =
  try set_geq a e; `Ok
  with Inconsistent -> `Inconsistent

let set_leq a (`Plus (b, k)) = set_geq b (`Plus (a, -k))
let set_lt a (`Plus (b, k)) = set_leq a (`Plus (b, k - 1))
let set_gt a (`Plus (b, k)) = set_geq a (`Plus (b, k + 1))

type path_weight = Negative_infinity | Weight of int

(* strongest consequence of current constraints on the variables
   given. *)
let limit vars =
  let vertices =
    let rec gather visited = function
      | [] -> visited
      | var :: vars ->
          if Set.mem visited var then
            gather visited vars
          else
            gather (Set.add visited var) vars
    in
    gather T.Set.empty (Set.to_list vars)
  in
  (* Floyd-Warshall algorithm for all-pairs longest-paths. *)
  let c =
    T.Set.fold vertices ~init:T.Map.empty ~f:(fun c u ->
      T.Map.add c ~key:u ~data:begin
        Id.Map.fold u.outgoing ~init:T.Map.empty
          ~f:(fun ~key:_ ~data:e c_u ->
            T.Map.add c_u ~key:e.target ~data:(Weight e.weight))
      end)
  in
  let get c u v =
    match T.Map.find c u with
    | None -> Negative_infinity
    | Some c_u ->
        match T.Map.find c_u v with
        | None -> Negative_infinity
        | Some k -> k
  in
  let set c u v k =
    let c_u =
      match T.Map.find c u with
      | None -> T.Map.empty
      | Some map -> map
    in
    T.Map.add c ~key:u ~data:(T.Map.add c_u ~key:v ~data:k)
  in
  let plus a b =
    match (a, b) with
    | (Weight a, Weight b) -> Weight (a + b)
    | (Negative_infinity, _)
    | (_, Negative_infinity) -> Negative_infinity
  in
  let greater_than a b =
    match (a, b) with
    | (Weight a, Weight b) -> a > b
    | (Weight _, Negative_infinity) -> true
    | (Negative_infinity, _) -> false
  in
  let c =
    Set.fold vertices ~init:c ~f:(fun c w ->
      Set.fold vertices ~init:c ~f:(fun c u ->
        Set.fold vertices ~init:c ~f:(fun c v ->
          let c_uv = get c u v in
          let c_uw = get c u w in
          let c_wv = get c w v in
          if greater_than (plus c_uw c_wv) c_uv then
            set c u v (plus c_uw c_wv)
          else
            c)))
  in
  Map.fold c ~init:[] ~f:(fun ~key:u ~data:c_u acc ->
    if Set.mem vars u then
      Map.fold c_u ~init:acc ~f:(fun ~key:v ~data:w acc ->
        if Set.mem vars v then
          match w with
          | Negative_infinity -> acc
          | Weight k -> `Geq (v, `Plus (u, k)) :: acc
        else
          acc)
    else
      acc)

include T

type s = [`Plus of t * int]
