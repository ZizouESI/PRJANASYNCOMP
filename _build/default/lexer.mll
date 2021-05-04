{
  open Lexing
  open Parser
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

rule token = parse
     | [' ''\t']       { token lexbuf }
     | '\n'            { next_line lexbuf; token lexbuf }
     | eof             { EOF }
     | "Var"   { VAR }
     | "Si" {SI}
     | "Alors" {ALORS}
     | "Sinon" {SINON}
     | "Debut" { BEGIN }
     | "Fin"   { END }
     | "Avance" {AVANCE}
     | "Tourne"	{TOURNE}
     | "BasPinceau"	{BASP}
     | "HautPinceau"	{HAUTP}
     | "Tant que" {TANTQUE}
     | "Faire" {FAIRE}
     | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as s     { IDENT s}
     | (['1'-'9']['0'-'9']*|'0') as i  { NOMBRE (int_of_string i) }
     | '+'     { PLUS }
     | '-'	{MINUS}
     | '*' {TIMES}
     | '/' {DIV}
     | '='     { AFFECT }
     | ';'	{SEMICOLON}
     | '('     { LPARA }
     | ')'     { RPARA }
     | _       { raise (Error (Lexing.lexeme lexbuf)) }
