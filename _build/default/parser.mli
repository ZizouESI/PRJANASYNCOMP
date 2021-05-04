
(* The type of tokens. *)

type token = 
  | VAR
  | TOURNE
  | TIMES
  | TANTQUE
  | SINON
  | SI
  | SEMICOLON
  | RPARA
  | PLUS
  | NOMBRE of (int)
  | MINUS
  | LPARA
  | IDENT of (string)
  | HAUTP
  | FAIRE
  | EOF
  | END
  | DIV
  | CHANGEEPAISSEUR
  | CHANGECOULEUR
  | BEGIN
  | BASP
  | AVANCE
  | ALORS
  | AFFECT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val s: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program)
