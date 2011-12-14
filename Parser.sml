local
type t__1__ = (int*int)
type t__2__ = char*(int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = string*(int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = string*(int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = int*(int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | CCHAR of t__2__
  | CHAR of t__3__
  | COMMA of t__4__
  | CSTRING of t__5__
  | ELSE of t__6__
  | EOF of t__7__
  | EQUAL of t__8__
  | ID of t__9__
  | IF of t__10__
  | INT of t__11__
  | LBRACKET of t__12__
  | LESS of t__13__
  | LPAR of t__14__
  | LSBRACKET of t__15__
  | MINUS of t__16__
  | NUM of t__17__
  | PLUS of t__18__
  | RBRACKET of t__19__
  | REF of t__20__
  | RETURN of t__21__
  | RPAR of t__22__
  | RSBRACKET of t__23__
  | SEMICOLON of t__24__
  | THEN of t__25__
  | WHILE of t__26__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* ASSIGN *),
  258 (* CCHAR *),
  259 (* CHAR *),
  260 (* COMMA *),
  261 (* CSTRING *),
  262 (* ELSE *),
  263 (* EOF *),
  264 (* EQUAL *),
  265 (* ID *),
  266 (* IF *),
  267 (* INT *),
  268 (* LBRACKET *),
  269 (* LESS *),
  270 (* LPAR *),
  271 (* LSBRACKET *),
  272 (* MINUS *),
  273 (* NUM *),
  274 (* PLUS *),
  275 (* RBRACKET *),
  276 (* REF *),
  277 (* RETURN *),
  278 (* RPAR *),
  279 (* RSBRACKET *),
  280 (* SEMICOLON *),
  281 (* THEN *),
  282 (* WHILE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\007\000\007\000\008\000\
\\008\000\006\000\004\000\004\000\005\000\005\000\013\000\013\000\
\\013\000\013\000\013\000\013\000\014\000\014\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\\009\000\010\000\010\000\011\000\011\000\012\000\012\000\012\000\
\\000\000";

val yylen = "\002\000\
\\002\000\007\000\000\000\001\000\001\000\002\000\000\000\003\000\
\\000\000\002\000\001\000\002\000\003\000\001\000\002\000\005\000\
\\007\000\003\000\004\000\005\000\000\000\002\000\001\000\001\000\
\\001\000\001\000\003\000\003\000\003\000\003\000\004\000\003\000\
\\003\000\000\000\001\000\001\000\003\000\001\000\002\000\004\000\
\\002\000";

val yydefred = "\000\000\
\\000\000\000\000\004\000\005\000\041\000\000\000\000\000\001\000\
\\011\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\024\000\025\000\000\000\000\000\009\000\000\000\
\\023\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\\008\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\015\000\000\000\002\000\
\\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\\000\000\032\000\018\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\013\000\000\000\031\000\040\000\000\000\022\000\019\000\
\\000\000\037\000\000\000\020\000\000\000\017\000";

val yydgoto = "\002\000\
\\005\000\006\000\007\000\031\000\032\000\018\000\014\000\015\000\
\\028\000\051\000\052\000\029\000\056\000\057\000";

val yysindex = "\006\000\
\\002\255\000\000\000\000\000\000\000\000\003\255\251\254\000\000\
\\000\000\007\255\009\255\000\000\000\000\008\255\002\255\066\255\
\\251\254\013\255\000\000\000\000\071\255\018\255\000\000\170\255\
\\000\000\170\255\026\255\149\255\041\255\002\255\042\255\000\000\
\\000\000\170\255\170\255\000\000\170\255\053\255\178\255\153\255\
\\170\255\170\255\170\255\170\255\170\255\000\000\170\255\000\000\
\\251\254\185\255\029\255\000\000\172\255\189\255\013\255\066\255\
\\033\255\000\000\000\000\196\255\000\000\031\255\040\255\040\255\
\\207\255\000\000\170\255\000\000\000\000\066\255\000\000\000\000\
\\066\255\000\000\047\255\000\000\066\255\000\000";

val yyrindex = "\000\000\
\\052\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\006\255\000\000\000\000\000\000\
\\000\000\038\255\000\000\000\000\080\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\093\255\052\255\240\254\000\000\
\\000\000\044\255\000\000\000\000\000\000\050\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\051\255\000\000\000\000\000\000\000\000\000\000\050\255\
\\000\000\000\000\000\000\000\000\106\255\136\255\119\255\132\255\
\\090\255\000\000\044\255\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\024\255\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\042\000\243\255\070\000\033\000\051\000\000\000\067\000\
\\233\255\028\000\000\000\000\000\240\255\043\000";

val YYTABLESIZE = 225;
val yytable = "\030\000\
\\039\000\017\000\040\000\009\000\003\000\014\000\001\000\014\000\
\\009\000\008\000\050\000\053\000\004\000\054\000\010\000\012\000\
\\009\000\060\000\061\000\062\000\063\000\064\000\013\000\065\000\
\\017\000\016\000\016\000\007\000\016\000\016\000\016\000\037\000\
\\016\000\016\000\016\000\016\000\033\000\016\000\042\000\041\000\
\\016\000\047\000\016\000\050\000\016\000\049\000\044\000\042\000\
\\045\000\016\000\068\000\072\000\077\000\075\000\019\000\003\000\
\\076\000\020\000\003\000\006\000\078\000\021\000\022\000\004\000\
\\023\000\034\000\024\000\019\000\021\000\025\000\020\000\048\000\
\\036\000\026\000\021\000\022\000\011\000\023\000\027\000\024\000\
\\038\000\066\000\025\000\038\000\034\000\035\000\026\000\038\000\
\\055\000\038\000\036\000\027\000\038\000\027\000\074\000\038\000\
\\026\000\038\000\071\000\000\000\026\000\038\000\038\000\038\000\
\\000\000\026\000\000\000\000\000\026\000\033\000\026\000\027\000\
\\027\000\027\000\026\000\026\000\026\000\000\000\033\000\000\000\
\\000\000\033\000\029\000\033\000\000\000\000\000\000\000\033\000\
\\033\000\033\000\000\000\029\000\000\000\000\000\029\000\028\000\
\\029\000\000\000\000\000\030\000\029\000\029\000\029\000\000\000\
\\028\000\000\000\000\000\028\000\030\000\028\000\000\000\000\000\
\\000\000\028\000\028\000\028\000\042\000\030\000\030\000\030\000\
\\042\000\043\000\000\000\000\000\044\000\043\000\045\000\000\000\
\\044\000\000\000\045\000\019\000\046\000\000\000\020\000\000\000\
\\059\000\000\000\021\000\042\000\000\000\000\000\000\000\024\000\
\\043\000\042\000\025\000\044\000\067\000\045\000\043\000\000\000\
\\042\000\044\000\069\000\045\000\042\000\043\000\000\000\058\000\
\\044\000\043\000\045\000\042\000\044\000\000\000\045\000\000\000\
\\043\000\000\000\070\000\044\000\000\000\045\000\042\000\000\000\
\\000\000\073\000\000\000\043\000\000\000\000\000\044\000\000\000\
\\045\000";

val yycheck = "\016\000\
\\024\000\015\000\026\000\009\001\003\001\022\001\001\000\024\001\
\\003\001\007\001\034\000\035\000\011\001\037\000\020\001\009\001\
\\011\001\041\000\042\000\043\000\044\000\045\000\014\001\047\000\
\\038\000\002\001\003\001\022\001\005\001\022\001\007\001\014\001\
\\009\001\010\001\011\001\012\001\024\001\014\001\008\001\014\001\
\\017\001\001\001\019\001\067\000\021\001\004\001\016\001\008\001\
\\018\001\026\001\022\001\019\001\006\001\070\000\002\001\003\001\
\\073\000\005\001\007\001\022\001\077\000\009\001\010\001\011\001\
\\012\001\022\001\014\001\002\001\019\001\017\001\005\001\030\000\
\\022\001\021\001\009\001\010\001\007\000\012\001\026\001\014\001\
\\001\001\049\000\017\001\004\001\014\001\015\001\021\001\008\001\
\\038\000\023\000\020\001\026\001\013\001\004\001\067\000\016\001\
\\004\001\018\001\056\000\255\255\008\001\022\001\023\001\024\001\
\\255\255\013\001\255\255\255\255\016\001\004\001\018\001\022\001\
\\023\001\024\001\022\001\023\001\024\001\255\255\013\001\255\255\
\\255\255\016\001\004\001\018\001\255\255\255\255\255\255\022\001\
\\023\001\024\001\255\255\013\001\255\255\255\255\016\001\004\001\
\\018\001\255\255\255\255\004\001\022\001\023\001\024\001\255\255\
\\013\001\255\255\255\255\016\001\013\001\018\001\255\255\255\255\
\\255\255\022\001\023\001\024\001\008\001\022\001\023\001\024\001\
\\008\001\013\001\255\255\255\255\016\001\013\001\018\001\255\255\
\\016\001\255\255\018\001\002\001\024\001\255\255\005\001\255\255\
\\024\001\255\255\009\001\008\001\255\255\255\255\255\255\014\001\
\\013\001\008\001\017\001\016\001\004\001\018\001\013\001\255\255\
\\008\001\016\001\023\001\018\001\008\001\013\001\255\255\022\001\
\\016\001\013\001\018\001\008\001\016\001\255\255\018\001\255\255\
\\013\001\255\255\022\001\016\001\255\255\018\001\008\001\255\255\
\\255\255\022\001\255\255\013\001\255\255\255\255\016\001\255\255\
\\018\001";

val yyact = vector_ 42 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 33 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : S100.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : S100.Prog))
;
(* Rule 2, file Parser.grm, line 38 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 6 : S100.Type
val d__2__ = peekVal 5 : S100.Sid
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : S100.Dec list
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : S100.Stat
val d__7__ = peekVal 0 : S100.FunDec list
in
( ((d__1__), (d__2__), (d__4__), (d__6__), (d__3__)) :: (d__7__) ) end : S100.FunDec list))
;
(* Rule 3, file Parser.grm, line 39 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( [] ) end : S100.FunDec list))
;
(* Rule 4, file Parser.grm, line 42 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( S100.Char (d__1__) ) end : S100.Type))
;
(* Rule 5, file Parser.grm, line 43 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( S100.Int (d__1__) ) end : S100.Type))
;
(* Rule 6, file Parser.grm, line 46 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Dec list
val d__2__ = peekVal 0 : S100.Dec
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 7, file Parser.grm, line 47 *)
val _ = update_ yyact 7
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 8, file Parser.grm, line 51 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Dec list
val d__2__ = peekVal 1 : S100.Dec
val d__3__ = peekVal 0 : (int*int)
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 9, file Parser.grm, line 52 *)
val _ = update_ yyact 9
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 10, file Parser.grm, line 55 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Type
val d__2__ = peekVal 0 : S100.Sid list
in
( ((d__1__), (d__2__)) ) end : S100.Dec))
;
(* Rule 11, file Parser.grm, line 58 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Val (d__1__) ) end : S100.Sid))
;
(* Rule 12, file Parser.grm, line 59 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : string*(int*int)
in
( S100.Ref (d__2__) ) end : S100.Sid))
;
(* Rule 13, file Parser.grm, line 63 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Sid
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Sid list
in
( (d__1__) :: (d__3__) ) end : S100.Sid list))
;
(* Rule 14, file Parser.grm, line 64 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Sid
in
( [(d__1__)] ) end : S100.Sid list))
;
(* Rule 15, file Parser.grm, line 67 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Exp
val d__2__ = peekVal 0 : (int*int)
in
( S100.EX (d__1__) ) end : S100.Stat))
;
(* Rule 16, file Parser.grm, line 69 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : S100.Exp
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : S100.Stat
in
( S100.If ((d__3__),(d__5__),(d__1__)) ) end : S100.Stat))
;
(* Rule 17, file Parser.grm, line 71 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 6 : (int*int)
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : S100.Exp
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : S100.Stat
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : S100.Stat
in
( S100.IfElse ((d__3__),(d__5__),(d__7__),(d__1__)) ) end : S100.Stat))
;
(* Rule 18, file Parser.grm, line 73 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( S100.Return ((d__2__),(d__1__)) ) end : S100.Stat))
;
(* Rule 19, file Parser.grm, line 75 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : S100.Dec list
val d__3__ = peekVal 1 : S100.Stat list
val d__4__ = peekVal 0 : (int*int)
in
( S100.Block ((d__2__),(d__3__),(d__1__)) ) end : S100.Stat))
;
(* Rule 20, file Parser.grm, line 77 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : S100.Exp
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : S100.Stat
in
( S100.While ((d__3__),(d__5__),(d__1__)) ) end : S100.Stat))
;
(* Rule 21, file Parser.grm, line 80 *)
val _ = update_ yyact 21
(fn () => repr(let
in
( [] ) end : S100.Stat list))
;
(* Rule 22, file Parser.grm, line 81 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Stat
val d__2__ = peekVal 0 : S100.Stat list
in
( (d__1__) :: (d__2__) ) end : S100.Stat list))
;
(* Rule 23, file Parser.grm, line 84 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( S100.NumConst (d__1__) ) end : S100.Exp))
;
(* Rule 24, file Parser.grm, line 85 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( S100.CharConst (d__1__) ) end : S100.Exp))
;
(* Rule 25, file Parser.grm, line 86 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.StringConst (d__1__) ) end : S100.Exp))
;
(* Rule 26, file Parser.grm, line 87 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Lval
in
( S100.LV (d__1__) ) end : S100.Exp))
;
(* Rule 27, file Parser.grm, line 89 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Lval
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Assign ((d__1__),(d__3__),(d__2__)) ) end : S100.Exp))
;
(* Rule 28, file Parser.grm, line 91 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Plus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 29, file Parser.grm, line 92 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Minus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 30, file Parser.grm, line 93 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Less ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 31, file Parser.grm, line 95 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : S100.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( S100.Call (#1 (d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 32, file Parser.grm, line 97 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : S100.Exp))
;
(* Rule 33, file Parser.grm, line 98 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Equal((d__1__),(d__3__),(d__2__)) ) end : S100.Exp))
;
(* Rule 34, file Parser.grm, line 102 *)
val _ = update_ yyact 34
(fn () => repr(let
in
( [] ) end : S100.Exp list))
;
(* Rule 35, file Parser.grm, line 103 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp list
in
( (d__1__) ) end : S100.Exp list))
;
(* Rule 36, file Parser.grm, line 106 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp
in
( [(d__1__)] ) end : S100.Exp list))
;
(* Rule 37, file Parser.grm, line 108 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp list
in
( (d__1__) :: (d__3__) ) end : S100.Exp list))
;
(* Rule 38, file Parser.grm, line 110 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Var (d__1__) ) end : S100.Lval))
;
(* Rule 39, file Parser.grm, line 111 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 1 : string*(int*int)
val d__2__ = peekVal 0 : (int*int)
in
( S100.Deref (d__1__) ) end : S100.Lval))
;
(* Rule 40, file Parser.grm, line 114 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : S100.Exp
val d__4__ = peekVal 0 : (int*int)
in
( S100.Lookup (#1 (d__1__),(d__3__),(d__2__)) ) end : S100.Lval))
;
(* Entry Prog *)
val _ = update_ yyact 41 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
