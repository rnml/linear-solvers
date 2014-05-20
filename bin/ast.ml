open Core.Std
open Solvers.Std

module Var : Identifiable = String

module Linear = struct
  include Linear

  let sexp_of_t t =
    match Linear.value t with
    | Some x -> Float.sexp_of_t x
    | None -> Sexp.Atom "unknown"

  let div a b =
    match value b with
    | Some b -> div a b
    | None -> failwith "non-constant denominator"

  let times a b =
    match value a with
    | Some a -> times a b
    | None ->
      match value b with
      | Some b -> times b a
      | None ->
        failwith "neither numerator nor denominator is constant"
end

module Env = struct
  type t = Linear.t Var.Table.t with sexp_of
  let create () = Var.Table.create ()
end

module Expr = struct
  type t =
  | Var    of Var.t
  | Const  of float
  | Times  of t * t
  | Div    of t * t
  | Plus   of t * t
  | Minus  of t * t
  | Negate of t
  with sexp_of

  let rec eval env = function
    | Var x -> Hashtbl.find_or_add env x ~default:(fun () -> Linear.var (Linear.Var.create ()))
    | Const x      -> Linear.const x
    | Times (a, b) -> Linear.times (eval env a) (eval env b)
    | Div   (a, b) -> Linear.div   (eval env a) (eval env b)
    | Plus  (a, b) -> Linear.plus  (eval env a) (eval env b)
    | Minus (a, b) -> Linear.minus (eval env a) (eval env b)
    | Negate a     -> Linear.negate (eval env a)

end

module Stmt = struct
  type t =
  | Equate of Expr.t * Expr.t * Expr.t list
  with sexp_of

  let eval env = function
    | Equate (a, b, cs) ->
      let eval x = Expr.eval env x in
      let a = eval a in
      let b = eval b in
      Linear.equate a b;
      List.iter cs ~f:(fun c -> Linear.equate b (eval c))

end
