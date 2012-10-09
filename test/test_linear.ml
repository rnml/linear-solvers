open Core.Std
open Solvers.Std

module L = Linear

(* open Int.Replace_polymorphic_compare *)

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

let equateS a k =
  try eq a k with L.Inconsistent | L.Redundant -> die ()
;;

let equateI a k =
  try eq a k; die () with L.Inconsistent -> () | L.Redundant -> die ()
;;

let equateR a k =
  try eq a k; die () with L.Inconsistent -> die () | L.Redundant -> ()
;;

let () =
  let x = L.var () in
  let y = L.var () in
  let z = L.var () in
  equateS (x - 3 * y + z) 4;
  equateS (2 * x - 8 * y + 8 * z) (-2);
  equateS (-6 * x + 3 * y - 16 * z) 9;
(*
  dump "x" x;
  dump "y" y;
  dump "z" z;
*)
;;

let () =
  let x = L.var () in
  let y = L.var () in
  let z = L.var () in
  equateS (2 * x - y + z) 1 ;
  equateS (3 * x + 2 * y - 4 * z) 4;
  equateI (-6 * x + 3 * y  - 3 * z) 2;
;;

let () =
  let x = L.var () in
  let y = L.var () in
  let z = L.var () in
  equateS (x - y + 2 * z) (-3) ;
  equateS (4 * x + 4 * y - 2 * z) 1;
  equateR (-2 * x + 2 * y - 4 * z) 6;
;;

