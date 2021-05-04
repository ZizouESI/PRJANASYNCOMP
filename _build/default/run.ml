(* programme principal *)

let print_position outx lexbuf =
  Lexing.(
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "Ligne %d Col %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  )
    
let _ =
  let lb = Lexing.from_channel stdin
  in
  try
    let _ast =
      Parser.s Lexer.token lb
    in  
    (*Call the checker*)
    Checker.check_program _ast;
    (*Call the interpreter*)
    (*let (env,inst)=Interpreter.exec_program _ast in
    *)
    print_string (Syntax.ast_to_string _ast);
    print_string "\n";
    (*
    print_string (Syntax.list_inst_to_string inst);
    print_string "\n";*)
    
    Interpreter.drawing _ast;

  with
  | Lexer.Error msg ->
     Printf.fprintf stderr "%a: Lexer error reading %s\n" print_position lb msg;
     exit (-1)
  | Parser.Error ->
     Printf.fprintf stderr "%a: Syntax error\n" print_position lb;
     exit (-1)
  | Checker.Error s ->
     Printf.fprintf stderr "Type error: %s\n" s;
     exit (-1)
  | Interpreter.Error s -> 
      Printf.fprintf stderr "Interpretation error: %s\n" s;
      exit(-1);
  | Graphics.Graphic_failure s -> 
    Printf.fprintf stderr "Drawing error: %s\n" s;
    exit(-1);
   ;;
 
