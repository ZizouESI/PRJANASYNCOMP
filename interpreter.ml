open Syntax
open Graphics
open Turtle 
open Formes


let bas=ref 0
let position=ref (init_position 0. 0. 90 0)

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
                


  




let rec list_assoc_to_string = function 
|[] -> ""
|[(e,v)] -> "("^e^","^ string_of_int (valI v)^")"
|(e,v)::l' -> "("^e^","^ string_of_int (valI v)^")" ^ "," ^ list_assoc_to_string l'
;;

(*let  transform_cmds cmds env= List.map (fun x -> match x with 
                                              |Avance(x) -> Line(valI(eval env x))
                                              |Tourne(x) -> Turn(valI(eval env x))
                                              |_ -> Nothing
                                      ) (List.filter (fun x -> match x with 
                                                              |Affectation(_,_) -> false
                                                              |Avance(_)-> true
                                                              |Cond(_,_,_) -> false
                                                              |Tourne(_) -> true
                                                              |BasPinceau -> false
                                                              |HautPinceau->false
                                                      ) cmds)
;;*)
 
let rec translate cmds bas env acc=
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
  
let draw d =
  let _border = 100 in
  let p= !position in
  let (p2,drawing) = executer_list_command p d in
  position := p2;
  (*Appel de fonctions initialement faites pour redimentionner les dessins automatiquement*)
  (*Dans le cas ou toutes les commandes sont executées toutes à la fois*)
  (*let  minx,_maxx,miny,_maxy = extremums_dessin drawing in*)
	(*let window_width = maxx - minx + 2*border in
	let window_height = maxy - miny + 2*border in*)
	let centered_drawing = map_dessin ( fun (x,y) ->
		(x+_border, y+_border)
	) drawing in
	(*resize_window window_width window_height;*)
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
|Loop(_e,_il)-> env

and exec_list env il = List.fold_left (exec_inst) env il

  let rec cmds_to_string = function
  |[]->""
  |x::l'-> match x with
          |Turn(x) -> "Turn("^string_of_int x^")" ^ "," ^ cmds_to_string l'
          |Move(x) -> "Move("^string_of_int x^")"^ "," ^ cmds_to_string l'
          |Line(x) -> "Line("^string_of_int x^")"^ "," ^ cmds_to_string l'


 let exec_program (ds,il) = 
  let _list_assoc = exec_list (initialiser ds) il in
            (*let list_inst = List.map (fun x -> match x with 
                                      |Cond(e,il1,il2) -> if valI(eval list_assoc e) <> 0 then Cond(e,il1,[]) else Cond(e,[],il2)
                                      |y -> y
                                      ) il 
            in *)
            print_string (list_assoc_to_string _list_assoc);
            ()

let drawing ast=
  
 (*let to_draw = translate cmds 0 env [] in
 print_string (cmds_to_string to_draw);
 print_string "\n";*)
  open_graph " 1000x700";
  (*let _p =draw [Turn(-90);Move(100);Line(200)] (init_position 0. 0. 90 0) in
  let _p2 =draw [Turn(-90);Line(200)] _p in*)
  exec_program ast;
  (*draw [Line(100)];
  draw [Turn(-90)];
  draw [Line(100)];
  draw [Turn(-90)];
  draw [Line(100)];
  draw [Turn(-90)];
  draw [Line(100)];*)
  ignore (wait_next_event [Button_down]);
	close_graph ()	
;;
