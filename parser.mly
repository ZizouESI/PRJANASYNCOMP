%token <int> NOMBRE
%token <string> IDENT
%token PLUS MINUS LPARA RPARA 
%token VAR SEMICOLON AVANCE TOURNE BASP HAUTP SI ALORS SINON TANTQUE FAIRE
%token AFFECT BEGIN END EOF
%start <Syntax.program> s
%left PLUS MINUS
%{ open Syntax %}
%%

s: p=program EOF                                     {p}

program: ds=declarations is=instruction              {(ds,is)}

declarations: 
  | VAR i=IDENT SEMICOLON ds=declarations               {i::ds}
  |{[]}
  

instruction:
  | BEGIN bi=blocInstructions END                    {bi}
  | SI e=expression ALORS i1=instruction SINON i2=instruction {[Cond(e,i1,i2)]}
  | AVANCE e=expression                              {[Avance(e)]} 
  | TOURNE e=expression								 {[Tourne(e)]}
  | i=IDENT AFFECT e=expression						 {[Affectation(i,e)]}
  | BASP    {[BasPinceau]}
  | HAUTP {[HautPinceau]}
  | TANTQUE e=expression FAIRE i=instruction {[Loop(e,i)]}



blocInstructions:	
  | i=instruction SEMICOLON bi=blocInstructions		  {i@bi}
  |{[]}

expression:
  | i=IDENT                                         {Ident(i)}
  | n=NOMBRE                                        {Nombre(n)}
  | LPARA e=expression RPARA                        {Bracketed(e)}
  | e1=expression PLUS e2=expression                {App(e1,Plus,e2)}
  | e1=expression MINUS e2=expression               {App(e1,Minus,e2)}


  



