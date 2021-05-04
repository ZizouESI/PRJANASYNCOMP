open Syntax

exception Error of string

(* extract a type environment from a declaration list *) 
let rec get_declarations = function
  | [] -> []
  | v::r -> let rl = get_declarations r in
                if List.exists (fun x -> x=v) rl
                then raise (Error ("Declared twice: "^v))
                else v::rl

(* types of binary operators *)
let type_operator = function
  | Plus | Minus |Times  | Div -> (Int,Int,Int)
  

(* compute the type of an expression *)
let rec type_expression decs = function
  | Nombre _ -> Int
  | Ident s -> begin
      try
        if List.exists (fun x -> s=x) decs then Int
        else raise(Not_found)
      with Not_found -> raise (Error ("Identifier not declared : "^s))

    end
  | Bracketed(e) -> type_expression decs e
  | App(e1,o,e2) ->
     let t1 = type_expression decs e1
     and t2 = type_expression decs e2
     and (to1,to2,tor) = type_operator o
     in 
      if t1 = to1 && t2 = to2
        then tor
        else raise (Error ("Type error in operator application"))

(* check the typing of an instruction *)
let rec check_instruction decs = function
  | HautPinceau -> ()
  | BasPinceau -> ()
  | Affectation(s,e) -> 
                          if  List.exists (fun x-> x=s) decs then 
                            if type_expression decs e = Int then 
                              ()
                            else
                              raise
                                 (Error ("Inconsistent types in assignment"))
                          else 
                             raise (Error ("variable not declared: "^s))
  | Avance e -> if type_expression decs e = Int then () else raise (Error ("Inconsistent type for Avance instruction"))
  | Tourne e -> if type_expression decs e = Int then () else raise (Error ("Inconsistent type for Tourne instruction"))
  | Cond(e,il1,il2)-> if type_expression decs e = Int then check_instructions decs (il1@il2) else raise (Error ("Inconsistent type for Condition instruction")) 
  | Loop(e,il)      -> if type_expression decs e = Int then check_instructions decs il else raise (Error ("Inconsistent type for Loop instruction"))
  | ChangeCouleur(_) -> ()
  | ChangeEpaisseur(_)-> ()
  
  (* check the typing of an instruction list *)
and check_instructions decs il =
  List.iter
    (function i -> check_instruction decs i)
    il

(* check the typing of a complete program *)
let check_program (dl,il) =
    check_instructions (get_declarations dl) il
