open Kaleidoscope_lib
(* open Llvm *)

(* top ::= definition | external | expression | ';' *)
let rec main_loop lexbuf =
  let prompto = fun () ->
    print_string "ready> "; flush stdout;
    main_loop lexbuf in
  try match Parser.toplevel Lexer.token lexbuf with
    | Ast.End -> ()
    | Ast.Sep ->
      Lexing.flush_input lexbuf;
      main_loop lexbuf
    | Ast.Definition e ->
      print_endline "parsed a function definition;";
      print_endline (Ast.show_func e);
      prompto ()
    | Ast.Extern e ->
      print_endline "parsed a extern;";
      print_endline (Ast.show_proto e);
      prompto ()
    | Ast.Expression e ->
      print_endline "parsed a top-level expr;";
      print_endline (Ast.show_expr e);
      prompto ()
  with
    Parsing.Parse_error ->
    Lexing.flush_input lexbuf; prompto ()

let main () =
  print_newline (); print_string "ready> "; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  main_loop lexbuf

let () = main ()
