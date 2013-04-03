open Core.Std
open Solvers.Std

module L = Linear

let main cin =
  let lexbuf = Lexing.from_channel cin in
  let stmts = Parser.main Lexer.token lexbuf in
  let env = Ast.Env.create () in
  List.iter stmts ~f:(Ast.Stmt.eval env);
  Ast.Env.sexp_of_t env
  |! Sexp.to_string_hum
  |! print_endline;
  ()

let command =
  Command.basic ~summary:"linear equation solver"
    Command.Spec.(empty +> anon (maybe ("FILE" %: file)))
    (fun file () ->
      match file with
      | None -> main stdin
      | Some file -> In_channel.with_file file ~f:main)

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

