type token =
  | NUM of (int64)
  | ID of (string)
  | DEF
  | ANDDEF
  | ADD1
  | SUB1
  | LPARENSPACE
  | LPARENNOSPACE
  | RPAREN
  | LBRACK
  | RBRACK
  | LET
  | IN
  | EQUAL
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | IF
  | COLON
  | ELSECOLON
  | EOF
  | PRINT
  | PRINTSTACK
  | TRUE
  | FALSE
  | ISBOOL
  | ISNUM
  | ISTUPLE
  | EQEQ
  | LESSSPACE
  | GREATER
  | LESSEQ
  | GREATEREQ
  | AND
  | OR
  | NOT
  | COLONEQ
  | SEMI
  | NIL
  | LAMBDA
  | BEGIN
  | END
  | SHADOW
  | REC
  | UNDERSCORE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Exprs

let full_span() = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span(start, endtok) = (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)

# 58 "parser.ml"
let yytransl_const = [|
  259 (* DEF *);
  260 (* ANDDEF *);
  261 (* ADD1 *);
  262 (* SUB1 *);
  263 (* LPARENSPACE *);
  264 (* LPARENNOSPACE *);
  265 (* RPAREN *);
  266 (* LBRACK *);
  267 (* RBRACK *);
  268 (* LET *);
  269 (* IN *);
  270 (* EQUAL *);
  271 (* COMMA *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* TIMES *);
  275 (* IF *);
  276 (* COLON *);
  277 (* ELSECOLON *);
    0 (* EOF *);
  278 (* PRINT *);
  279 (* PRINTSTACK *);
  280 (* TRUE *);
  281 (* FALSE *);
  282 (* ISBOOL *);
  283 (* ISNUM *);
  284 (* ISTUPLE *);
  285 (* EQEQ *);
  286 (* LESSSPACE *);
  287 (* GREATER *);
  288 (* LESSEQ *);
  289 (* GREATEREQ *);
  290 (* AND *);
  291 (* OR *);
  292 (* NOT *);
  293 (* COLONEQ *);
  294 (* SEMI *);
  295 (* NIL *);
  296 (* LAMBDA *);
  297 (* BEGIN *);
  298 (* END *);
  299 (* SHADOW *);
  300 (* REC *);
  301 (* UNDERSCORE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\007\000\007\000\
\006\000\006\000\006\000\006\000\006\000\006\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\009\000\009\000\009\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\016\000\016\000\015\000\015\000\005\000\
\005\000\005\000\005\000\017\000\008\000\008\000\018\000\018\000\
\019\000\019\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\005\000\003\000\005\000\
\004\000\005\000\006\000\003\000\003\000\001\000\001\000\003\000\
\002\000\002\000\004\000\004\000\005\000\005\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\001\000\004\000\001\000\004\000\004\000\
\003\000\003\000\003\000\008\000\008\000\005\000\008\000\008\000\
\005\000\001\000\001\000\006\000\007\000\001\000\003\000\001\000\
\001\000\003\000\003\000\001\000\001\000\002\000\001\000\003\000\
\000\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\076\000\000\000\000\000\000\000\000\000\
\000\000\074\000\001\000\031\000\005\000\006\000\000\000\000\000\
\000\000\000\000\008\000\012\000\002\000\003\000\009\000\010\000\
\011\000\007\000\004\000\000\000\058\000\000\000\000\000\000\000\
\046\000\059\000\000\000\000\000\072\000\026\000\000\000\000\000\
\025\000\000\000\000\000\069\000\000\000\000\000\000\000\000\000\
\068\000\000\000\000\000\064\000\065\000\000\000\000\000\000\000\
\075\000\032\000\033\000\034\000\041\000\039\000\037\000\040\000\
\038\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\070\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\021\000\
\000\000\049\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\027\000\000\000\067\000\066\000\000\000\000\000\017\000\
\000\000\000\000\045\000\000\000\048\000\047\000\060\000\063\000\
\000\000\000\000\000\000\057\000\030\000\000\000\000\000\054\000\
\029\000\018\000\000\000\000\000\000\000\024\000\061\000\000\000\
\000\000\000\000\000\000\000\000\014\000\019\000\000\000\000\000\
\000\000\000\000\016\000\056\000\055\000\053\000\052\000"

let yydgoto = "\002\000\
\004\000\029\000\030\000\050\000\074\000\099\000\089\000\052\000\
\032\000\100\000\033\000\034\000\069\000\035\000\075\000\005\000\
\053\000\006\000\007\000"

let yysindex = "\006\000\
\011\255\000\000\014\255\000\000\029\255\011\255\077\001\021\255\
\011\255\000\000\000\000\000\000\000\000\000\000\142\255\183\255\
\010\255\077\001\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\077\001\000\000\027\255\038\000\099\255\
\000\000\000\000\020\255\013\255\000\000\000\000\019\255\025\255\
\000\000\024\255\037\255\000\000\017\255\017\255\043\255\006\255\
\000\000\034\255\036\255\000\000\000\000\045\255\026\255\077\001\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\077\001\069\255\169\000\077\001\069\255\
\047\255\054\255\063\255\017\255\017\255\077\001\000\000\210\000\
\017\255\017\255\077\001\000\000\251\000\070\255\071\255\000\000\
\068\255\075\255\077\001\077\001\077\001\000\000\073\255\000\000\
\051\255\000\000\084\255\074\255\079\255\196\255\077\001\017\255\
\064\255\091\255\092\255\093\255\000\000\095\255\097\255\098\255\
\100\255\000\000\101\255\000\000\000\000\077\001\077\001\000\000\
\096\255\104\255\000\000\077\001\000\000\000\000\000\000\000\000\
\077\001\102\255\106\255\000\000\000\000\115\255\118\255\000\000\
\000\000\000\000\103\255\017\255\077\001\000\000\000\000\077\001\
\077\001\077\001\077\001\006\255\000\000\000\000\110\255\111\255\
\112\255\136\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\118\001\000\000\000\000\000\000\036\001\118\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\127\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\137\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\143\255\000\000\000\000\085\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\140\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\144\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\228\255\239\255\251\255\007\000\211\255\
\084\000\180\255\000\000\000\000\000\000\089\000\216\255\000\000\
\000\000\150\000\154\000"

let yytablesize = 671
let yytable = "\051\000\
\044\000\031\000\090\000\110\000\086\000\087\000\001\000\044\000\
\115\000\040\000\043\000\044\000\054\000\003\000\044\000\008\000\
\045\000\046\000\044\000\045\000\046\000\073\000\055\000\045\000\
\046\000\076\000\077\000\070\000\036\000\071\000\081\000\082\000\
\009\000\079\000\056\000\106\000\107\000\057\000\078\000\080\000\
\111\000\112\000\042\000\083\000\088\000\084\000\091\000\142\000\
\047\000\092\000\095\000\085\000\047\000\048\000\049\000\047\000\
\072\000\049\000\070\000\047\000\071\000\049\000\096\000\128\000\
\093\000\101\000\103\000\094\000\104\000\011\000\012\000\105\000\
\108\000\013\000\014\000\015\000\016\000\113\000\116\000\117\000\
\118\000\123\000\125\000\129\000\043\000\120\000\121\000\122\000\
\119\000\126\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\127\000\124\000\130\000\131\000\132\000\090\000\133\000\
\026\000\134\000\135\000\027\000\136\000\137\000\140\000\149\000\
\138\000\139\000\058\000\059\000\060\000\148\000\156\000\157\000\
\158\000\144\000\051\000\143\000\141\000\145\000\022\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\146\000\150\000\
\068\000\147\000\151\000\152\000\153\000\154\000\011\000\012\000\
\159\000\062\000\013\000\014\000\015\000\016\000\038\000\023\000\
\013\000\017\000\155\000\102\000\015\000\097\000\037\000\010\000\
\018\000\000\000\000\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\027\000\039\000\028\000\011\000\
\012\000\000\000\000\000\013\000\014\000\015\000\016\000\041\000\
\000\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\000\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\058\000\059\000\060\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\027\000\042\000\028\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\000\000\044\000\000\000\044\000\044\000\044\000\000\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\000\000\044\000\044\000\
\000\000\044\000\044\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\000\000\042\000\000\000\042\000\042\000\042\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\000\000\
\042\000\042\000\000\000\042\000\042\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\000\000\043\000\
\043\000\043\000\000\000\043\000\000\000\000\000\000\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\043\000\043\000\000\000\043\000\043\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\000\000\022\000\022\000\022\000\000\000\022\000\000\000\000\000\
\000\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\022\000\000\000\022\000\
\022\000\011\000\012\000\000\000\000\000\013\000\014\000\015\000\
\016\000\098\000\000\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\000\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\027\000\
\000\000\028\000\011\000\012\000\000\000\000\000\013\000\014\000\
\015\000\016\000\109\000\000\000\000\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\
\027\000\000\000\028\000\011\000\012\000\000\000\000\000\013\000\
\014\000\015\000\016\000\114\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\000\000\027\000\000\000\028\000\071\000\071\000\071\000\000\000\
\071\000\071\000\071\000\071\000\000\000\000\000\000\000\071\000\
\000\000\000\000\000\000\000\000\000\000\000\000\071\000\000\000\
\000\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\
\000\000\000\000\071\000\000\000\071\000\011\000\012\000\000\000\
\000\000\013\000\014\000\015\000\016\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\027\000\000\000\028\000\073\000\073\000\
\000\000\000\000\073\000\073\000\073\000\073\000\000\000\000\000\
\000\000\073\000\000\000\000\000\000\000\000\000\000\000\000\000\
\073\000\000\000\000\000\073\000\073\000\073\000\073\000\073\000\
\073\000\073\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\073\000\000\000\000\000\073\000\000\000\073\000"

let yycheck = "\017\000\
\000\000\007\000\048\000\080\000\045\000\046\000\001\000\002\001\
\085\000\015\000\016\000\002\001\018\000\003\001\002\001\002\001\
\007\001\008\001\002\001\007\001\008\001\009\001\028\000\007\001\
\008\001\007\001\008\001\008\001\008\001\010\001\007\001\008\001\
\004\001\009\001\008\001\076\000\077\000\000\000\020\001\015\001\
\081\000\082\000\000\000\020\001\002\001\009\001\013\001\124\000\
\043\001\014\001\056\000\015\001\043\001\044\001\045\001\043\001\
\037\001\045\001\008\001\043\001\010\001\045\001\068\000\104\000\
\020\001\071\000\020\001\042\001\015\001\001\001\002\001\009\001\
\078\000\005\001\006\001\007\001\008\001\083\000\009\001\009\001\
\013\001\009\001\009\001\020\001\000\000\091\000\092\000\093\000\
\014\001\011\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\103\000\015\001\009\001\009\001\009\001\148\000\009\001\
\036\001\009\001\009\001\039\001\009\001\009\001\015\001\140\000\
\118\000\119\000\016\001\017\001\018\001\015\001\009\001\009\001\
\009\001\020\001\140\000\129\000\021\001\020\001\000\000\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\020\001\141\000\
\038\001\020\001\144\000\145\000\146\000\147\000\001\001\002\001\
\009\001\009\001\005\001\006\001\007\001\008\001\009\001\009\001\
\013\001\012\001\148\000\072\000\013\001\069\000\009\000\006\000\
\019\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\036\001\255\255\255\255\039\001\040\001\041\001\001\001\
\002\001\255\255\255\255\005\001\006\001\007\001\008\001\009\001\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\019\001\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\016\001\017\001\018\001\255\255\255\255\
\255\255\255\255\036\001\255\255\255\255\039\001\040\001\041\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\255\255\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\255\255\038\001\039\001\
\255\255\041\001\042\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\255\255\009\001\255\255\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\255\255\
\038\001\039\001\255\255\041\001\042\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\013\001\255\255\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\036\001\255\255\038\001\039\001\255\255\041\001\042\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\255\255\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001\255\255\255\255\039\001\255\255\041\001\
\042\001\001\001\002\001\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\255\255\255\255\039\001\
\255\255\041\001\001\001\002\001\255\255\255\255\005\001\006\001\
\007\001\008\001\009\001\255\255\255\255\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\019\001\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\036\001\255\255\255\255\
\039\001\255\255\041\001\001\001\002\001\255\255\255\255\005\001\
\006\001\007\001\008\001\009\001\255\255\255\255\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\036\001\255\255\
\255\255\039\001\255\255\041\001\001\001\002\001\003\001\255\255\
\005\001\006\001\007\001\008\001\255\255\255\255\255\255\012\001\
\255\255\255\255\255\255\255\255\255\255\255\255\019\001\255\255\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\036\001\
\255\255\255\255\039\001\255\255\041\001\001\001\002\001\255\255\
\255\255\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\019\001\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\036\001\255\255\255\255\039\001\255\255\041\001\001\001\002\001\
\255\255\255\255\005\001\006\001\007\001\008\001\255\255\255\255\
\255\255\012\001\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\036\001\255\255\255\255\039\001\255\255\041\001"

let yynames_const = "\
  DEF\000\
  ANDDEF\000\
  ADD1\000\
  SUB1\000\
  LPARENSPACE\000\
  LPARENNOSPACE\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  LET\000\
  IN\000\
  EQUAL\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  IF\000\
  COLON\000\
  ELSECOLON\000\
  EOF\000\
  PRINT\000\
  PRINTSTACK\000\
  TRUE\000\
  FALSE\000\
  ISBOOL\000\
  ISNUM\000\
  ISTUPLE\000\
  EQEQ\000\
  LESSSPACE\000\
  GREATER\000\
  LESSEQ\000\
  GREATEREQ\000\
  AND\000\
  OR\000\
  NOT\000\
  COLONEQ\000\
  SEMI\000\
  NIL\000\
  LAMBDA\000\
  BEGIN\000\
  END\000\
  SHADOW\000\
  REC\000\
  UNDERSCORE\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 26 "parser.mly"
        ( ENumber(_1, full_span()) )
# 443 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
         ( EBool(true, full_span()) )
# 449 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
          ( EBool(false, full_span()) )
# 455 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                   ( ENil(full_span()) )
# 461 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
         ( Add1 )
# 467 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
         ( Sub1 )
# 473 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
        ( Not )
# 479 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
          ( Print )
# 485 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
           ( IsBool )
# 491 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
          ( IsNum )
# 497 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
            ( IsTuple )
# 503 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
               ( PrintStack )
# 509 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                    ( [(_1, _3, full_span())] )
# 517 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'bindings) in
    Obj.repr(
# 43 "parser.mly"
                                   ( (_1, _3, tok_span(1, 3))::_5 )
# 526 "parser.ml"
               : 'bindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'namebind) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                        ( [(_1, _3, full_span())] )
# 534 "parser.ml"
               : 'namebindings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'namebind) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'namebindings) in
    Obj.repr(
# 47 "parser.mly"
                                           ( (_1, _3, tok_span(1, 3))::_5 )
# 543 "parser.ml"
               : 'namebindings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bindings) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                         ( ELet(_2, _4, full_span()) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'namebindings) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                                 ( ELetRec(_3, _5, full_span()) )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                                      ( EIf(_2, _4, _6, full_span()) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                   ( _2 )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                         ( ESeq(_1, _3, full_span()) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 55 "parser.mly"
               ( _1 )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
         ( [_1] )
# 597 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 59 "parser.mly"
                     ( _1::_3 )
# 605 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                         ( ETuple([], full_span()) )
# 611 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                       ( ETuple([], full_span()) )
# 617 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                    ( ETuple([_2], full_span()) )
# 624 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                  ( ETuple([_2], full_span()) )
# 631 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 66 "parser.mly"
                                          ( ETuple(_2::_4, full_span()) )
# 639 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 67 "parser.mly"
                                        ( ETuple(_2::_4, full_span()) )
# 647 "parser.ml"
               : 'tuple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                   ( EId(_1, full_span()) )
# 654 "parser.ml"
               : 'id))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
         ( Plus )
# 660 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
          ( Minus )
# 666 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
          ( Times )
# 672 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
        ( And )
# 678 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
       ( Or )
# 684 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ( Greater )
# 690 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
              ( GreaterEq )
# 696 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
              ( Less )
# 702 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( LessEq )
# 708 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
         ( Eq )
# 714 "parser.ml"
               : 'prim2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'prim2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_operand) in
    Obj.repr(
# 86 "parser.mly"
                                              ( EPrim2(_2, _1, _3, full_span()) )
# 723 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_operand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 87 "parser.mly"
                                                   (
      match _1 with
      | EGetItem(lhs, idx, _) -> ESetItem(lhs, idx, _3, full_span())
      | _ -> raise Parsing.Parse_error
    )
# 735 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_operand) in
    Obj.repr(
# 92 "parser.mly"
                             ( _1 )
# 742 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prim1) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                                    ( EPrim1(_1, _3, full_span()) )
# 750 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_expr) in
    Obj.repr(
# 98 "parser.mly"
               ( _1 )
# 757 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'binop_operand) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                     ( EGetItem(_1, _3, full_span()) )
# 765 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'binop_operand) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 101 "parser.mly"
                                                                 ( EApp(_1, _3, Unknown, full_span()) )
# 773 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_operand) in
    Obj.repr(
# 102 "parser.mly"
                                                           ( EApp(_1, [], Unknown, full_span()) )
# 780 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                            ( _2 )
# 787 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                              ( _2 )
# 794 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'binds) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                                                                      ( ELambda(_4, _7, full_span()) )
# 802 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'binds) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                                                                    ( ELambda(_4, _7, full_span()) )
# 810 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                                           ( ELambda([], _4, full_span()) )
# 817 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'binds) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                                                                    ( ELambda(_4, _7, full_span()) )
# 825 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'binds) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                                                                  ( ELambda(_4, _7, full_span()) )
# 833 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                                         ( ELambda([], _4, full_span()) )
# 840 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 114 "parser.mly"
          ( _1 )
# 847 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'id) in
    Obj.repr(
# 115 "parser.mly"
       ( _1 )
# 854 "parser.ml"
               : 'binop_operand))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                           ( DFun(_2, [], _6, full_span()) )
# 862 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'binds) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                                 ( DFun(_2, _4, _7, full_span()) )
# 871 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bind) in
    Obj.repr(
# 122 "parser.mly"
         ( [_1] )
# 878 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binds) in
    Obj.repr(
# 123 "parser.mly"
                     ( _1::_3 )
# 886 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'namebind) in
    Obj.repr(
# 126 "parser.mly"
             ( _1 )
# 893 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'blankbind) in
    Obj.repr(
# 127 "parser.mly"
              ( _1 )
# 900 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binds) in
    Obj.repr(
# 128 "parser.mly"
                               ( BTuple(_2, full_span()) )
# 907 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binds) in
    Obj.repr(
# 129 "parser.mly"
                             ( BTuple(_2, full_span()) )
# 914 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                          ( BBlank(full_span()) )
# 920 "parser.ml"
               : 'blankbind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                  ( BName(_1, false, full_span()) )
# 927 "parser.ml"
               : 'namebind))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                         ( BName(_2, true, full_span()) )
# 934 "parser.ml"
               : 'namebind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 139 "parser.mly"
         ( [_1] )
# 941 "parser.ml"
               : 'declgroup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declgroup) in
    Obj.repr(
# 140 "parser.mly"
                          ( _1::_3 )
# 949 "parser.ml"
               : 'declgroup))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
    ( [] )
# 955 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declgroup) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 144 "parser.mly"
                    ( _1::_2 )
# 963 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                   ( Program(_1, _2, full_span()) )
# 971 "parser.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Lexing.position * Lexing.position) Exprs.program)
;;