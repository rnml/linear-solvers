open Core.Std

module L = Linear_regression

TEST_MODULE = struct

  let ( * ) k b = L.times (Float.of_int k) b
  let ( + ) = L.plus
  let ( - ) a b = a + L.negate b

  let dump s t =
    match L.value t with
    | None -> print_endline (s ^ " = NONE")
    | Some v -> print_endline (s ^ " = " ^ Float.to_string v)

  let eq a k = L.equate a (L.const (Float.of_int k))

  TEST_UNIT = (* exactly solvable #1 *)
    let x = L.Var.create () in
    begin
      let x = L.var x in
      eq x 4;
    end;
    match L.value x with
    | Some 4.0 -> ()
    | _ -> assert false

  TEST_UNIT = (* exactly solvable #2 *)
    let x = L.Var.create () in
    begin
      let x = L.var x in
      eq (2 * x) 4;
    end;
    match L.value x with
    | Some 2.0 -> ()
    | _ -> assert false

  TEST_UNIT = (* overdetermined *)
    let x = L.Var.create () in
    begin
      let x = L.var x in
      eq (2 * x) 4;
      eq (3 * x) 4;
    end;
    match L.value x with
    | Some v -> assert (v =. 20. /. 13.)
    | _ -> assert false

  TEST_UNIT = (* underdeterimined *)
    let x = L.Var.create () in
    let y = L.Var.create () in
    begin
      let x = L.var x in
      let y = L.var y in
      eq (x + y) 4;
    end;
    match (L.value x, L.value y) with
    | (None, None) -> ()
    | _ -> assert false

  TEST_UNIT =
    (* http://en.wikipedia.org/wiki/Simple_linear_regression#Numerical_example *)
    let a = L.Var.create () in
    let b = L.Var.create () in
    begin
      let a = L.var a in
      let b = L.var b in
      let point (x, y) = L.equate (L.plus a (L.times x b)) (L.const y) in
      let xs = [ 1.47;  1.50;  1.52;  1.55;  1.57;  1.60;  1.63;  1.65;  1.68;  1.70;  1.73;
                 1.75;  1.78;  1.80;  1.83] in
      let ys = [52.21; 53.12; 54.48; 55.84; 57.20; 58.57; 59.93; 61.29; 63.11; 64.47; 66.28;
                68.10; 69.92; 72.19; 74.46] in
      List.iter ~f:point (List.zip_exn xs ys)
    end;
    match (L.value a, L.value b) with
    | (Some a, Some b) ->
      let expected = "-39.062 x + 61.272" in
      let observed = sprintf "%.3f x + %.3f" a b in
      assert (String.equal expected observed)
    | _ -> assert false

end
