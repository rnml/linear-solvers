open Core.Std
open Solvers.Std

open Int.Replace_polymorphic_compare

module D = Diff_logic


let ge a b = D.set_geq a b
let le a b = D.set_leq a b
let lt a b = D.set_lt a b
let gt a b = D.set_gt a b

let n = ref 0

let die () =
  print_endline ("ERROR! \
    open the file in vi and run the following two line command\n\
    /ANC" ^ "HOR\n" ^ Int.to_string !n ^ "/[ynk]ay");
  assert false;
;;

let yay r a b = (incr n; match r a b with `Ok -> () | `Inconsistent -> die ())
let nay r a b = (incr n; match r a b with `Ok -> die () | `Inconsistent -> ())
let kay   a k = (incr n; if k <> D.lower_bound a then die ())

(* ANCHOR *)

let () =
  let x = D.var () in
  kay x 0;
  yay le x (`Plus (x, 0));
  kay x 0;
;;

let () =
  let x = D.var () in
  yay ge x (`Plus (x, 0));
  kay x 0;
;;

let () =
  let x = D.var () in
  nay lt x (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  nay gt x (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  yay gt x (`Plus (y, 0));
  kay x 1;
  kay y 0;
  nay ge y (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  yay lt x (`Plus (y, 0));
  kay x 0;
  kay y 1;
  nay le y (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  yay le x (`Plus (y, 0));
  kay x 0;
  kay y 0;
  nay lt y (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  yay ge x (`Plus (y, 0));
  kay x 0;
  kay y 0;
  nay gt y (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 0;
  yay ge y (`Plus (z, 1));
  kay x 2;
  kay y 1;
  kay z 0;
  nay ge z (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 0;
  yay ge y (`Plus (z, 1));
  kay x 2;
  kay y 1;
  kay z 0;
  yay ge z (`Plus (x, -2));
  kay x 2;
  kay y 1;
  kay z 0;
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 0;
  yay ge y (`Plus (z, 1));
  kay x 2;
  kay y 1;
  kay z 0;
  nay ge z (`Plus (x, -1));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 0;
  yay ge y (`Plus (z, 1));
  kay x 2;
  kay y 1;
  kay z 0;
  nay ge z (`Plus (x, 0));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 0;
  yay ge z (`Plus (x, 0));
  kay x 1;
  kay y 0;
  kay z 1;
  nay ge y (`Plus (z, 1));
;;

let () =
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge z (`Plus (x, 0));
  kay x 0;
  kay y 0;
  kay z 0;
  yay ge x (`Plus (y, 1));
  kay x 1;
  kay y 0;
  kay z 1;
  nay ge y (`Plus (z, 1));
;;

let () =
  let w = D.var () in
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  kay w 0;
  kay x 0;
  kay y 0;
  kay z 0;
  yay lt w (`Plus (x, 0));
  kay w 0;
  kay x 1;
  kay y 0;
  kay z 0;
  yay le y (`Plus (z, 0));
  kay w 0;
  kay x 1;
  kay y 0;
  kay z 0;
  yay lt x (`Plus (y, 0));
  kay w 0;
  kay x 1;
  kay y 2;
  kay z 2;
  nay le z (`Plus (x, 0));
;;

let () =
  let w = D.var () in
  let x = D.var () in
  let y = D.var () in
  let z = D.var () in
  yay lt w (`Plus (x, 0));
  kay w 0;
  kay x 1;
  kay y 0;
  kay z 0;
  yay le y (`Plus (z, 0));
  kay w 0;
  kay x 1;
  kay y 0;
  kay z 0;
  yay lt x (`Plus (y, 0));
  kay w 0;
  kay x 1;
  kay y 2;
  kay z 2;
  yay le z (`Plus (x, 2));
  kay w 0;
  kay x 1;
  kay y 2;
  kay z 2;
;;

let () =
  let x1 = D.var () in
  let x2 = D.var () in
  let x3 = D.var () in
  let x4 = D.var () in
  let x5 = D.var () in
  let x6 = D.var () in
  let x7 = D.var () in
  let x8 = D.var () in
  let x9 = D.var () in
  let x10 = D.var () in
  let x11 = D.var () in
  let x12 = D.var () in
  let x13 = D.var () in
  let x14 = D.var () in
  let x15 = D.var () in
  let x16 = D.var () in
  let x17 = D.var () in
  let x18 = D.var () in
  let x19 = D.var () in
  let x20 = D.var () in
  let x21 = D.var () in
  let x22 = D.var () in
  let x23 = D.var () in
  let x24 = D.var () in
  let x25 = D.var () in

  let axiom s1 s2 = yay lt s1 (`Plus (s2, 0)) in
  let rule s1 s2 s3 = yay le s1 (`Plus (s3, 0)); yay le s2 (`Plus (s3, 0)) in

  axiom x1 x2;
  axiom x4 x5;
  axiom x6 x7;
  axiom x11 x15;
  axiom x12 x17;
  axiom x13 x20;
  axiom x14 x22;
  axiom x10 x25;

  rule x2 x1 x3;
  rule x5 x7 x8;
  rule x4 x3 x6;
  rule x10 x15 x16;
  rule x16 x17 x18;
  rule x18 x10 x19;
  rule x10 x20 x21;
  rule x21 x22 x23;
  rule x19 x23 x24;
  rule x25 x24 x9;
;;

