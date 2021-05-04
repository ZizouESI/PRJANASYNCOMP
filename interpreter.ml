open Syntax
open Graphics
open Turtle 
open Formes

exception Error of string

let bas=ref 0
let position=ref (init_position 0. 0. 0 0)

type value = I of int

let valI = function I n -> n 

let initialiser ds = List.map (fun x -> (x,I(0))) ds

let rec eval env = function
|Nombre(n) -> I(n)
|Ident (i) -> List.assoc i env 
|Bracketed (e) -> eval env e
|App(e1,op,e2) -> let v1 = eval env e1 and v2 = eval env e2 in
                  match op with 
                  |Plus -> I((valI v1) + (valI v2))
                  |Minus -> I((valI v1) - (valI v2))
                  |Times -> I((valI v1) * (valI v2))
                  |Div -> if (valI v2) <> 0 then I((valI v1) / (valI v2)) else raise (Error ("Division par zero !"))


  




let rec list_assoc_to_string = function 
|[] -> ""
|[(e,v)] -> "("^e^","^ string_of_int (valI v)^")"
|(e,v)::l' -> "("^e^","^ string_of_int (valI v)^")" ^ "," ^ list_assoc_to_string l'
;;


 
(*let rec translate cmds bas env acc=
  match cmds with 
  |[] -> acc
  |x::l' -> match x with 
            |Affectation(_,_) -> translate l' bas env acc
            |Cond(_,il1,il2) -> if il1 <> [] then 
                                  let exec_il1= translate il1 0 env [] in 
                                  translate l' bas env (acc@ exec_il1)
                                else 
                                  let exec_il2= translate il2 0 env [] in 
                                  translate l' bas env (acc@exec_il2)
            |Loop(_,_) -> translate l' bas env acc
            |Tourne(y)-> translate l' bas env (acc@[Turn(valI(eval env y))])
            |BasPinceau -> translate l' 1 env acc
            |HautPinceau -> translate l' 0 env acc
            |Avance(y) -> if bas = 0 then translate l' bas env (acc@[Move(valI(eval env y))]) else translate l' bas env (acc@[Line(valI(eval env y))])
  *)
let draw d =
  let _border = 30 in
  let p= !position in
  let (p2,drawing) = executer_list_command p d in
  position := p2;
  
	let centered_drawing = map_dessin ( fun (x,y) ->
		(x+_border, y+_border)
	) drawing in
	
	dessiner centered_drawing;	
  
  ;;


let rec exec_inst env = function
|HautPinceau -> bas := 0 ; env
|BasPinceau -> bas := 1 ; env
|Affectation(s,e) -> List.map (fun (n, v) -> if n=s then (n,eval env e) else (n,v)) env 
|Avance (_e) -> ( match !bas with
                | 0 ->  draw [Move(valI(eval env _e))]; env  
                | 1 -> draw [Line(valI(eval env _e))] ; env
                |_ -> env )
|Tourne (_e) -> draw [Turn(valI(eval env _e))] ; env
|Cond(e,il1,il2) -> if valI(eval env e) <> 0 then exec_list env il1 else exec_list env il2
|Loop(_e,_il)-> while valI(eval env _e) <> 0 do let _res =exec_list env _il in () done; env
|ChangeCouleur(e)-> set_color (valI(eval env e)) ; env
|ChangeEpaisseur(e) -> Graphics.set_line_width (valI(eval env e)) ; env

and exec_list env il = List.fold_left (exec_inst) env il

  let rec cmds_to_string = function
  |[]->""
  |x::l'-> match x with
          |Turn(x) -> "Turn("^string_of_int x^")" ^ "," ^ cmds_to_string l'
          |Move(x) -> "Move("^string_of_int x^")"^ "," ^ cmds_to_string l'
          |Line(x) -> "Line("^string_of_int x^")"^ "," ^ cmds_to_string l'


 let exec_program (ds,il) = 
  let _list_assoc = exec_list (initialiser ds) il in
    ()

let drawing ast=
  open_graph " 1000x800";
  
  exec_program ast;

  ignore (wait_next_event [Button_down]);
	close_graph ()	
;;
