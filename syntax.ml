(* Syntaxe abstraite *)

(* Opérateurs binaires *)
type op = Plus | Minus |Times |Div

type typ = Int


(* Expressions *)
type expression =
  | Nombre of int
  | Ident of string 
  | Bracketed of expression 
  | App of expression * op * expression
  




(* Instructions *)
type instruction =
  | Affectation of string * expression
  | Avance of expression
  | Tourne of expression
  | Cond of expression * instruction list * instruction list
  | Loop of expression * instruction list
  | ChangeCouleur of string
  | ChangeEpaisseur of expression
  | HautPinceau
  | BasPinceau
  

type blocInstructions =  instruction list

(* Declaration d'un variable *)
type declaration = string

(* Programm complet *)
type program = declaration list * instruction list


let rec list_to_string = function 
|[] -> ""
|[e] -> e
| e::l ->  let str = e ^ "," ^ list_to_string l in 
           "["^str^"]"
          

let rec expression_to_string = function
|Nombre(n) -> "Nombre("^string_of_int n^")"
|Ident(i)->   "Ident("^i^")"
|Bracketed(e)-> "Bracketed("^ expression_to_string e^")"
|App(e1,op,e2)-> match op with
                 |Plus -> expression_to_string e1 ^"+"^ expression_to_string e2 
                 |Minus -> expression_to_string e1 ^"-"^ expression_to_string e2
                 |Times -> expression_to_string e1 ^"*"^ expression_to_string e2
                 |Div -> expression_to_string e1 ^"/"^ expression_to_string e2

let rec list_inst_to_string = function
|[] -> ""
| x::l' -> match x with 
          |Affectation(i,e) -> "Affectation("^i^","^expression_to_string e^")" ^ "," ^ list_inst_to_string l'
          |Avance(e) -> "Avance("^expression_to_string e^")"^ "," ^ list_inst_to_string l'
          |Tourne(e) -> "Tourne("^expression_to_string e^")"^ "," ^ list_inst_to_string l'
          |Cond(e,i1,i2) -> "Cond("^expression_to_string e^",il1="^list_inst_to_string i1^"il2="^list_inst_to_string i2^")"^","^ list_inst_to_string l'
          |HautPinceau -> "HautPinceau"^ "," ^ list_inst_to_string l'
          |BasPinceau -> "BasPinceau"^ "," ^ list_inst_to_string l'
          |Loop(e,l) -> "Loop("^expression_to_string e^","^list_inst_to_string l^")"^","^ list_inst_to_string l'
          |ChangeCouleur(e) -> "ChangeCouleur("^e^")"^ "," ^ list_inst_to_string l'
          |ChangeEpaisseur(e) -> "ChangeEpaisseur("^expression_to_string e^")"^ "," ^ list_inst_to_string l'

let ast_to_string ast =
  let first = fst(ast) in
  let second = snd(ast) in
  let dec=list_to_string first in
  let list_inst= list_inst_to_string second in
  dec ^ "\n" ^ "[" ^ list_inst ^ "]"