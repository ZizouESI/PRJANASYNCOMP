
module MenhirBasics = struct
  
  exception Error
  
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
    | NOMBRE of (
# 1 "parser.mly"
       (int)
# 20 "parser.ml"
  )
    | MINUS
    | LPARA
    | IDENT of (
# 2 "parser.mly"
       (string)
# 27 "parser.ml"
  )
    | HAUTP
    | FAIRE
    | EOF
    | END
    | DIV
    | BEGIN
    | BASP
    | AVANCE
    | ALORS
    | AFFECT
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState46
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState28
  | MenhirState26
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState15
  | MenhirState12
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState3
  | MenhirState0

# 11 "parser.mly"
   open Syntax 
# 77 "parser.ml"

let rec _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_goto_blocInstructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (Syntax.instruction list))), _, (bi : (Syntax.instruction list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.instruction list) = 
# 36 "parser.mly"
                                                   (i@bi)
# 163 "parser.ml"
         in
        _menhir_goto_blocInstructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (bi : (Syntax.instruction list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.instruction list) = 
# 24 "parser.mly"
                                                     (bi)
# 181 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPARA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) = 
# 42 "parser.mly"
                                                    (Bracketed(e))
# 218 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) = 
# 45 "parser.mly"
                                                    (App(e1,Times,e2))
# 237 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | ALORS | EOF | FAIRE | MINUS | PLUS | RPARA | SEMICOLON | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) = 
# 43 "parser.mly"
                                                    (App(e1,Plus,e2))
# 256 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) = 
# 46 "parser.mly"
                                                    (App(e1,Div,e2))
# 273 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | ALORS | EOF | FAIRE | MINUS | PLUS | RPARA | SEMICOLON | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) = 
# 44 "parser.mly"
                                                    (App(e1,Minus,e2))
# 292 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.expression) = 
# 47 "parser.mly"
                                                    (App(Nombre(0),Minus,e))
# 309 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | EOF | SEMICOLON | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.instruction list) = 
# 27 "parser.mly"
                                ([Tourne(e)])
# 332 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | FAIRE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AVANCE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | BASP ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | BEGIN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | HAUTP ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | IDENT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | SI ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TANTQUE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TOURNE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ALORS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AVANCE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | BASP ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | BEGIN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | HAUTP ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | IDENT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | SI ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TANTQUE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TOURNE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | EOF | SEMICOLON | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 2 "parser.mly"
       (string)
# 447 "parser.ml"
            ))), _, (e : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.instruction list) = 
# 28 "parser.mly"
                                      ([Affectation(i,e)])
# 453 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | EOF | SEMICOLON | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.instruction list) = 
# 26 "parser.mly"
                                                     ([Avance(e)])
# 482 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.instruction list) = 
# 37 "parser.mly"
   ([])
# 499 "parser.ml"
     in
    _menhir_goto_blocInstructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AVANCE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | BASP ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | BEGIN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | HAUTP ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | IDENT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | SI ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TANTQUE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TOURNE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | END ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SINON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AVANCE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | BASP ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | BEGIN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | HAUTP ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | SI ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TANTQUE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TOURNE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (i1 : (Syntax.instruction list))), _, (i2 : (Syntax.instruction list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.instruction list) = 
# 25 "parser.mly"
                                                              ([Cond(e,i1,i2)])
# 591 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (i : (Syntax.instruction list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.instruction list) = 
# 31 "parser.mly"
                                             ([Loop(e,i)])
# 603 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (ds : (string list))), _, (is : (Syntax.instruction list))) = _menhir_stack in
        let _v : (Syntax.program) = 
# 16 "parser.mly"
                                                     ((ds,is))
# 613 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (p : (Syntax.program))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.program) = 
# 14 "parser.mly"
                                                     (p)
# 628 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Syntax.program)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "parser.mly"
       (int)
# 646 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 1 "parser.mly"
       (int)
# 654 "parser.ml"
    )) = _v in
    let _v : (Syntax.expression) = 
# 41 "parser.mly"
                                                    (Nombre(n))
# 659 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parser.mly"
       (string)
# 704 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 2 "parser.mly"
       (string)
# 712 "parser.ml"
    )) = _v in
    let _v : (Syntax.expression) = 
# 40 "parser.mly"
                                                    (Ident(i))
# 717 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parser.mly"
       (string)
# 786 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AFFECT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | LPARA ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | MINUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NOMBRE _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.instruction list) = 
# 30 "parser.mly"
          ([HautPinceau])
# 825 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AVANCE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | BASP ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | BEGIN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | HAUTP ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | SI ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TANTQUE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TOURNE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | END ->
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.instruction list) = 
# 29 "parser.mly"
            ([BasPinceau])
# 866 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPARA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOMBRE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (i : (
# 2 "parser.mly"
       (string)
# 899 "parser.ml"
        ))), _, (ds : (string list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (string list) = 
# 19 "parser.mly"
                                                        (i::ds)
# 906 "parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AVANCE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | BASP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | BEGIN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | HAUTP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | IDENT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | SI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | TANTQUE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | TOURNE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 20 "parser.mly"
   ([])
# 1021 "parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | AVANCE | BASP | BEGIN | HAUTP | IDENT _ | SI | TANTQUE | TOURNE ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and s : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | AVANCE | BASP | BEGIN | HAUTP | IDENT _ | SI | TANTQUE | TOURNE ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 1099 "parser.ml"
