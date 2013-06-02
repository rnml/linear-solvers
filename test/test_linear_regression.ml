open Core.Std
open Solvers.Std

module L = Linear_regression

let ( * ) k b = L.times (Float.of_int k) b
let ( + ) = L.plus
let ( - ) a b = a + L.negate b

let dump s t =
  match L.value t with
  | None -> print_endline (s ^ " = NONE")
  | Some v -> print_endline (s ^ " = " ^ Float.to_string v);
;;

let n = ref 0
let die () =
  print_endline begin "error: " ^ Int.to_string !n end;
  failwith "bad"
;;

let eq a k = incr n; L.equate a (L.const (Float.of_int k))

let () =
  let x = L.Var.create () in
  begin
    let x = L.var x in
    eq x 4;
  end;
  match L.value x with
  | Some 4.0 -> ()
  | _ -> assert false
;;

let () =
  let x = L.Var.create () in
  begin
    let x = L.var x in
    eq (2 * x) 4;
  end;
  match L.value x with
  | Some 2.0 -> ()
  | _ -> assert false
;;

let () =
  let x = L.Var.create () in
  begin
    let x = L.var x in
    eq (2 * x) 4;
    eq (3 * x) 4;
  end;
  match L.value x with
  | Some v -> assert (v =. 20. /. 13.)
  | _ -> assert false
;;

let () =
  let x = L.Var.create () in
  let y = L.Var.create () in
  begin
    let x = L.var x in
    let y = L.var y in
    eq (2 * x + y)     4;
    eq (3 * x + 2 * y) 4;
    eq (4 * x + 3 * y) 4;
  end;
  match (L.value x, L.value y) with
  | (Some _, Some _) -> assert false
  | _ -> assert false
;;

