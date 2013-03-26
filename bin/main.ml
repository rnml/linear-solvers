open Core.Std
open Solvers.Std

module L = Linear

module P = Parser

(* open Int.Replace_polymorphic_compare *)

let ( * ) k b = L.times (Float.of_int k) b
let ( + ) = L.plus
let ( - ) a b = a + L.negate b

let dump s t =
  match L.value t with
  | None   -> print_endline (s ^ " = NONE")
  | Some v -> print_endline (s ^ " = " ^ Float.to_string v)

let n = ref 0

let die () =
  print_endline begin "error: " ^ Int.to_string !n end;
  failwith "bad"

let eq a k = incr n; L.equate a (L.const (Float.of_int k))

let equateS a k =
  try eq a k with L.Inconsistent | L.Redundant -> die ()

let () =
  let x = L.var () in
  let y = L.var () in
  let z = L.var () in
        (* sq.ft      lot size *)
  equateS (1600 * x + 23000 * y) 540;
  equateS (1861 * x +  6969 * y) 450;
  equateS (1813 * x +  7405 * y - z) 0;
  dump "X" x;
  dump "Y" y;
  dump "Z" z;

