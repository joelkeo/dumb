open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors

let t name program input expected = name>::test_run ~args:[] ~std_input:input Naive program name expected;;
let tr name program input expected = name>::test_run ~args:[] ~std_input:input Register program name expected;;
let ta name program input expected = name>::test_run_anf ~args:[] ~std_input:input program name expected;;
let tgc name heap_size program input expected = name>::test_run ~args:[string_of_int heap_size] ~std_input:input Naive program name expected;;
let tvg name program input expected = name>::test_run_valgrind ~args:[] ~std_input:input Naive program name expected;;
let tvgc name heap_size program input expected = name>::test_run_valgrind ~args:[string_of_int heap_size] ~std_input:input Naive program name expected;;
let terr name program input expected = name>::test_err ~args:[] ~std_input:input Naive program name expected;;
let tgcerr name heap_size program input expected = name>::test_err ~args:[string_of_int heap_size] ~std_input:input Naive program name expected;;
let tanf name program input expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;

let tparse name program expected = name>::fun _ ->
  assert_equal (untagP expected) (untagP (parse_string name program)) ~printer:string_of_program;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

(* let tfvs name program expected = name>:: *)
(*   (fun _ -> *)
(*     let ast = parse_string name program in *)
(*     let anfed = anf (tag ast) in *)
(*     let vars = free_vars_P anfed [] in *)
(*     let c = Stdlib.compare in *)
(*     let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in *)
(*     assert_equal (List.sort c vars) (List.sort c expected) ~printer:str_list_print) *)
(* ;; *)

(* We could not figure out how to add builtins according to this implementation *)
let builtins_size = 0 (* arity + 0 vars + codeptr + padding *) * 1 (* TODO FIXME (List.length Compile.native_fun_bindings) *)

let pair_tests = [
  t "tup1" "let t = (4, (5, 6)) in
            begin
              t[0] := 7;
              t
            end" "" "(7, (5, 6))";
  t "tup2" "let t = (4, (5, nil)) in
            begin
              t[1] := nil;
              t
            end" "" "(4, nil)";
  t "tup3" "let t = (4, (5, nil)) in
            begin
              t[1] := t;
              t
            end" "" "(4, <cyclic tuple 1>)";
  t "tup4" "let t = (4, 6) in
            (t, t)"
           ""
           "((4, 6), (4, 6))"

]

let oom = [
  tgcerr "oomgc1" (7 + builtins_size) "(1, (3, 4))" "" "Out of memory";
  tgc "oomgc2" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))";
  (* tvgc "oomgc3" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))"; *)
  tgc "oomgc4" (4 + builtins_size) "(3, 4)" "" "(3, 4)";
  tgcerr "oomgc5" (3 + builtins_size) "(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)" "" "Allocation";
]
let gc = [
  tgc "gc_lam1" (10 + builtins_size)
      "let f = (lambda: (1, 2)) in
       begin
         f();
         f();
         f();
         f()
       end"
      ""
      "(1, 2)";
  ]

let input = [
    t "input1" "let x = input() in x + 2" "123" "125"
  ]


let desugar_tests = [
  t "desugaring_1" "let (x, y) = (2, 3) in x + y" "" "5";
  t "desugaring_2" "def f((x, y), z): x + y + z f((2, 3), 4)" "" "9";
  t "desugaring_3" "let ((x, y), z) = ((1, 2), 3) in x + y + z" "" "6";
  t "desugaring_4" "let (x, y) = ((2, 3), 4) in x[0] + x[1] + y" "" "9";
  t "desugaring_5" "let ((x, _), y) = ((2, 3), 4) in x + y" "" "6";
  terr "desugaring_err1" "def f((x, y), z): x + y + z f((2,), 4)" "" "index too large to get";
  t "desugaring_6" "let x = (0, 100) in begin x[0] := x[0] + 1 ; x[0] end" "" "1";
];;

let is_well_formed_tests = [
  terr "iwft_unbound_id" "x" "" "is not in scope";
  terr "iwft_overflow" "x + 4611686018427387904" "" "not supported in this language";
  terr "iwft_shadowing" "let x = 3, x = 4 in x" "" "duplicates one at";
  terr "iwft_func_redefined" "def hello(x): x def hello(y): y 4" "" "hello, redefined";
  terr "iwft_duplicate_param" "def f(x, x): x 1" "" "x, redefined";
  terr "iwft_arity_mismatch" "def f(x): x f(2, 2)" "" "expected an arity of 1";
  terr "iwft_unbound_function" "f(2)" "" "is not in scope";
  terr "iwft_param_unbound" "def f(x): x f(x)" "" "The identifier x";
  terr "iwft_shadowing2" "let x = 3, x = y in 1" "" "The identifier y";
];;
  

let lib_tests = [
  t "lib_1" "def append(l1, l2): if l1 == nil: l2 else: (l1[0], append(l1[1], l2)) append((3, (2, nil)), (1, nil))" "" "(3, (2, (1, nil)))";
  t "lib_2" "def sum(l): if l == nil: 0 else: l[0] + sum(l[1]) sum((3, (4, (3, nil))))" "" "10";
  t "lib_3" "def append(l1, l2): if l1 == nil: l2 else: (l1[0], append(l1[1], l2)) def reverse(l): if l == nil: nil else: append(reverse(l[1]), (l[0], nil)) reverse((1, (2, (3, nil))))" "" "(3, (2, (1, nil)))";
  t "lib_4" "def link(first, rest): (first, rest) link(1, link(2, link(3, nil)))" "" "(1, (2, (3, nil)))";
];;
let y_combinator_tests = [
  t "y_combinator_1" "(lambda (n1, n2, cnt, x): if cnt == 0: n1 else: x(n2, n1 + n2, cnt - 1, x))(1, 1, 25, (lambda (n1, n2, cnt, x): if cnt == 0: n1 else: x(n2, n1 + n2, cnt - 1, x)))" "" "121393";
]
let legacy = [
  t "test_is_bool1" "isbool(true)" "" "true";
  t "test_is_bool2" "isbool(false)" "" "true";
  t "test_is_bool3" "isbool(0)" "" "false";
  t "test_is_bool4" "isbool(123)" "" "false";
  t "test_is_bool5" "isbool((0,123))" "" "false";
  t "test_is_bool6" "isbool((true,123))" "" "false";
  t "test_is_bool7" "isbool((123,123))" "" "false";
  t "test_is_bool8" "isbool((false,123))" "" "false";

  t "test_is_tuple1" "istuple(true)" "" "false";
  t "test_is_tuple2" "istuple(false)" "" "false";
  t "test_is_tuple3" "istuple(0)" "" "false";
  t "test_is_tuple4" "istuple(123)" "" "false";
  t "test_is_tuple5" "istuple((0,123))" "" "true";
  t "test_is_tuple6" "istuple((true,123))" "" "true";
  t "test_is_tuple7" "istuple((123,123))" "" "true";
  t "test_is_tuple8" "istuple((false,123))" "" "true";

  t "test_is_num1" "isnum(true)" "" "false";
  t "test_is_num2" "isnum(false)" "" "false";
  t "test_is_num3" "isnum(0)" "" "true";
  t "test_is_num4" "isnum(123)" "" "true";
  t "test_is_num5" "isnum((0,123))" "" "false";
  t "test_is_num6" "isnum((true,123))" "" "false";
  t "test_is_num7" "isnum((123,123))" "" "false";
  t "test_is_num8" "isnum((false,123))" "" "false";
  t "simple_test1" "let x = 3 in x" "" "3";
  t "simple_test2" "3 * 4 * 5" "" "60";
  t "simple_test3" "let x = if sub1(55) < 54: false else: true in x" "" "true";
  t "simple_test3.5" "let x = if true: (if (3>2): 1 else: 2) else: 3 in  x" "" "1";
  t "simple_test4" "let x = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: true in x" "" "true";
  t "if1" "if 7 < 8: 5 else: 3" "" "5";
  t "if2" "if 0 > 1: 4 else: 2" "" "2";
  terr "overflow" "add1(5073741823000000000)" "" "not supported";
  t "prim1_1" "add1(1)" "" "2";
  t "prim1_2" "add1(3)" "" "4";
  t "prim1_3" "add1(-1)" "" "0";
  t "prim1_4" "sub1(-1)" "" "-2";
  t "prim1_5" "add1(4611686018427387902)" "" "4611686018427387903";
  t "prim1_6" "sub1(-4611686018427387903)" "" "-4611686018427387904";
  terr "prim1_fail_1" "add1(false)" "" "arithmetic expected a number, got false";
  terr "prim1_fail_2" "add1(true)" "" "arithmetic expected a number, got true";
  terr "prim1_fail_3" "add1(4611686018427387903)" "" "Integer overflow";
  terr "prim1_fail_4" "sub1(-4611686018427387904)" "" "Integer overflow";
  t "not_1" "!(true)" "" "false";
  t "not_2" "!(false)" "" "true";
  t "not_3" "!(!(false))" "" "false";
  terr "not_fail_1" "!(3)" "" "logic expected a boolean, got 3";
  terr "not_fail_2" "!(add1(1))" "" "logic expected a boolean, got 2";
  t "prim2_1" "(1 + 2)" "" "3";
  t "prim2_2" "(1 - 2)" "" "-1";
  t "prim2_3" "(4 * 8)" "" "32";
  t "prim2_4" "(-9 * 10)" "" "-90";
  t "prim2_5" "(4611686018427387902 * 1)" "" "4611686018427387902";
  terr "prim2_fail_1" "(4611686018427387902 * 10000)" "" "Integer overflow" ;
  t "prim2cmp_1" "(1 > 2)" "" "false";
  t "prim2cmp_2" "(2 > 1)" "" "true";
  t "prim2cmp_3" "(1 >= 0)" "" "true";
  t "prim2cmp_4" "(4 < 3)" "" "false";
  t "prim2cmp_5" "(-9 <= -9)" "" "true";
  t "prim2cmp_6" "(-4611686018427387902 <= -4611686018427387903)" "" "false";
  terr "prim2cmp_fail_1" "(2 < false)" "" "arithmetic expected a number";
  terr "prim2cmp_fail_2" "(1 >= (if true: true else: 3))" "" "arithmetic expected a number";
  t "if_1" "if true: 1 else: 2" "" "1";
  t "if_2" "if false: add1(2) else: add1(3)" "" "4";
  t "if_3" "if false: (1 + 2 * 8) else: (1 - (2 * 8))" "" "-15";
  t "if_4" "if false: (true + false) else: true" "" "true";
  terr "if_fail_1" "if 1: 2 else: 3" "" "if expected";
  terr "if_fail_2" "if (if true: 1 else: false): 2 else: 3" "" "if expected";
  t "and_1" "true && true" "" "true";
  t "and_2" "false && true" "" "false";
  t "and_3" "true && false" "" "false";
  t "and_4" "false && false" "" "false";
  t "and_5" "false && (3 + 5)" "" "false";
  t "and_6" "if (false && (2 + 3)): 1 else: 2" "" "2";
  t "or_1" "true || true" "" "true";
  t "or_2" "false || true" "" "true";
  t "or_3" "true || false" "" "true";
  t "or_4" "false || false" "" "false";
  t "or_5" "true || (2 * false)" "" "true";
  terr "and_fail_1" "1 && true" "" "logic expected a boolean,";
  terr "and_fail_2" "true && (1 * 4)" "" "logic expected a boolean,";
  terr "or_fail_1" "false || (2 * 3)" "" "logic expected a boolean,";
]

let basic_gc_tests = [
  tgc "bgct1" 8 "let junkly = (if true: (9, 10, 11)[0] else: false) in
  let tup1 = (1, 290, 3) in let tup2 = (4, 5, 6) in tup1[1] + tup2[0]" "" "294";
  tgc "bgcc1" 20 "let junk = if true: let jl = (lambda(t): t - 9) in jl(10) else: false in
  let l1 = (lambda (x): x) in let l2 = (lambda (y): y) in 
                   let big = (lambda (x, y, z): let huh = (lambda (n): n - 1) in huh(x + z)) in
                   let l3 = (lambda (n): n) in
                   big(l1(4), 5, junk)" "" "4";
  tgc "bgcm1" 40 "let a = (lambda (n): n) in
  let b = (lambda (n, x): n + (x, x)[0]) in
  let c = a(7) + b(3, 4) in
  let d = (1, 2, 3, 5) in
  c + d[0]" "" "15";
  tgc "stack_frame_test_1" 24
  "let f1 = (lambda (n): n + 1) in let f2 = (lambda (x): (x, (2, 3))[0] + 2) in let a = f2(7) in f1(1)" "" "2";
  tgc "bgcm2" 12 "let wasteful_id = (lambda (n): (1, 1, 1, n, 1, 1, 1)[3]) in
                  let a = wasteful_id(99) in
                  let b = wasteful_id(101) in
                  let c = wasteful_id(7) in
                  a + b + c" "" "207" ;
  tgc "s1" 12 "let make_garbage = (lambda (n): let a = (n, n, n) in a[0]) in
  let some_garbage = make_garbage(9) in let more_garbage = make_garbage(2) in
  let z = (1, 2, 3) in let y = z in let p = (8, 9, 10) in p[0] + y[2] + some_garbage + more_garbage" "" "22"
]

let fwd_ptr_tests = [
  tgc "fwd_ptr_tuple_1" 12
  "let junk = (if true: (1, 2, 33, 4, 5)[2] else: false) in let a = (1, 2, 3) in
  let b = a in let c = b in let d = c in let e = d in let f = (true, true) in let last = (false, false) in a[2]" "" "3";
  tgc "fwd_ptr_closure_1" 12 
  "let junk = (if true: (lambda (n): n * n)(6) else: false) in
  let f1 = (lambda (n): n + 7) in let f2 = f1 in let f3 = f2 in let f4 = (lambda (n): n - 9) in let t = (1, 2) in f3(10)" "" "17";
  tgc "cfpt_1" 12 "let t1 = (1, 2, 3) in let t2 = (t1, t1, t1) in let t3 = (9, 10) in  10" "" "10"
]
let tons_of_garbage_tests = [
  tgc "tons_of_garbage_1" 20 "let gen_garbage = (lambda (n): (1, 2, 3, 4, 5, 6, 7, 8, 9)[0]) in
  let rec loop = (lambda (n): if n == 0: (10, 11) else: begin gen_garbage(n) ; loop(sub1(n)) end)
  in let res = loop(1000)
  in let trigger_more_garabage_coll = (1, 2) in
  res[1]"
  "" "11" ;
  tgc "tons_of_garbage_2" 40 "let starter = (2, 3) in 
  let recd = (1, nil) in
  let hidden_rec_update_1 = (if true: begin recd[1] := (9, nil) ; false end else: false) in
  let hidden_rec_update_2 = (if true: begin recd[1][1] := (19, nil) ; false end else: false) in
  let generate_garbage = (lambda (n): (1, 2, 3, 4, 5, 7, 8, 9)) in
  let rec loop = (lambda (x): if x == 0: recd[1][1][0] else: let bogus = (if true: generate_garbage(x)[0] else: false) in let a = 2 in loop(sub1(x))) in
    loop(1000)" "" "19"
]
let recursive_gc_tests = [
  tgc "rtt1" 36 "let x = (1, 2, 3) in let y = (x, x, x) in let z = (1, 2) in y[1][1]" "" "2";
  tgc "recursive_test_1" 36 "let x = (1, 2, 3) in
                             let bogus = if true: begin x[0] := (7, 8, 9); 6 end else: 7 in 
                             let filler1 = (false, false, false) in let filler2 = 88 in
                             x[0][2]" "" "9" ;
  tgc "linked_list_1" 24 "(0, (1, (2, (3, (4, (5, nil))))))[1][1][0]" "" "2" ;
  tgc "linked_list_2" 36 "let rec gen_list = (lambda (n): if n == 0:
    nil
    else: let rest = gen_list(n - 1) in let a = 43 in (n, rest)) in
  let junk = (if true: (91, 92, 93, 94, 95, 96, 97, 98, 99)[7] else: true) in
  let l2 = gen_list(8) in
  l2[1][1][1][0]" "" "5"; 
  tgc "linked_list_2_better" 24 "let rec gen_list = (lambda (n): if n == 0:
    nil
    else: let rest = gen_list(n - 1) in (n, rest)) in
  let junkk = (if true: gen_list(5)[0]  else: false) in
  let l = gen_list(5) in
  l[1][1][1][0]" "" "2"                          
]

let custom_oom_tests = [
  tgcerr "cot1" 10 "let tup1 = (1, 2, 3, 4, 5) in let dummy = (if true: begin tup1[3] := (7, 8, 9) ; false end else: false) in
  let a = 7 in
  (4,)" "" "Out of memory"]
let let_rec_tests = [
t "let_rec_1" "let rec fib = (lambda (n1, n2, cnt): if cnt == 0 : n1 else: fib(n2, n1 + n2, cnt - 1)) in  fib(1, 1, 25)" "" "121393" ;
t "let_rec_2" "let rec f1 = (lambda (x): if x == 10: x else: f2(x + 1)), 
                        f2 = (lambda (x): if x == 10: x * 100 else: f1(x + 1)) in
                    f1(1)" "" "1000"; 
t "let_rec_3" "let rec f1 = (lambda (x): f2(x, x + 1)),
                        f2 = (lambda (y, z): if y == 10: 100 else: f1(z)) 
                        in f1(0)" "" "100"
]

let simpler_tup_tests = [
  t "s_tup1" "(1, 3)[1]" "" "3";
  t "s_tup2" "(1, 3)[0]" "" "1";
  t "s_tup3" "(1, 3, 4, 5, 6)[4]" "" "6";
  t "s_tup4" "nil" "" "nil";
  t "s_tup5" "(3, 5)" "" "(3, 5)";
  t "s_tup6" "((1, 2), (3, 4), (5, 6))" "" "((1, 2), (3, 4), (5, 6))";
  terr "s_tup7" "(1, 2, 3)[3]" "" "index too large";
  terr "s_tup8" "(1, 2)[-1]" "" "index too small";
  terr "s_tup9" "let x = 3 in x[0]" "" "get expected tuple";
  terr "s_tup10" "let x = (1, 2) in x + 3" "" "expected a number, got";
  t "s_tup11" "let t = (1, 2, 3) in begin t[0] := 7; t[0] end" "" "7";
  terr "s_tup12" "let t = (1, 2, 3) in begin t[0] := 7; t[4] end" "" "index too large";
  terr "s_tup13" "let t = (1, 2, 3) in begin t[0] := 7; t[-1] end" "" "index too small";
  terr "s_tup14" "let t = 1 in begin t[0] := 7; t[-1] end" "" "set expected tuple";
  t "s_tup15" "let t = (1, 2, 3) in begin t[0] := (1, 2, 3); t[0] end" "" "(1, 2, 3)";
  t "s_tup16" "istuple((2, 3, 4))" "" "true"; 
  t "s_tup17" "istuple(2)" "" "false";
  t "s_tup18" "istuple(true)" "" "false";
  t "s_tup19" "isbool((1, 2))" "" "false";
  t "s_tup20" "isbool(false)" "" "true";
  terr "s_tup21" "3[0]" "" "get expected tuple";
];;

let nullary_function = [
  t "nullary_1" "let a = 17 in let nullary = (lambda: a + 3) in nullary()" "" "20"
]

let small_tuple = [
  t "size_1_tuple" "let s1 = (6,) in s1[0]" "" "6" ;
  t "size_zero_tuple" "let s0 = () in istuple(s0)" "" "true"
]

let builtins = [
  t "print1" "print(4)" "" "4\n4";
]

let simple_reg_acc = [
  tr "sra1" "let x = 3 in let y = 2 in let z = y in let p = x in z" "" "2";
  tr "sra2" "let a = 2 in let x = (if true: let p = 2 in let z = 7 in z else: let b = a in a) in let t = a in a" "" "2";
]

let reg_acc_lambda = [
  tr "sral1" "let a = 2 in let f = (lambda (x): let b = x + a in b + a) in f(a + 1)" "" "7" ;
  tr "sral2" "let a = 2 in let f1 = (lambda (x): let f2 = (lambda (y): y * a) in f2(x)) in f1(78)" "" "156" ;
  tr "sral3" "let a = 6 in (lambda (x): (lambda (x): (lambda (x): (lambda (x): let p = 3 in p + x + a)(x))(x))(x))(4)" "" "13" ;
  tr "sral4" "let f = (lambda (x): let r12sb = 2 in x * r12sb) in let r12sb = 7 in f(r12sb) + r12sb" "" "21"
]

let reg_acc_many = [
  tr "ram1" "let a = 1 in let b = 10 in let c = 100 in let d = 1000 in
            let e = 10000 in let f = 100000 in let temp = b + c + d + e + f in temp + a" "" "111111" ;
  tr "ram2" "(lambda (x): let a = 1 in let b = 10 in let c = 100 in let d = 1000 in
  let e = 10000 in let f = 100000 in let temp = b + c + d + e + f in temp + a + x)(2)" "" "111113" ;
  tr "ram3" "let z = 3 in let y = 5 in let op = 1 in let f = (lambda (x): let a = 1 in let b = 10 in let c = 100 in let d = 1000 in
  let e = 10000 in let f = 100000 in let temp = b + c + d + e + f in temp + a + x)
in let o = 1000000 in f(z) + o + y + op" "" "1111120"
]

let reg_acc_if = [
  tr "rai1" "if true: (if true: (if true: let result = 1 in result else: false) else: let y = 2 in y) else: let z = 3 in z" "" "1" ;
  tr "rai2" "if true: (if true: (if true: let small = 1 in 
                                         let a = 10 in
                                         let b = 100 in
                                         let c = 1000 in
                                         let d = 10000
                                         in a + b + c + d + small
    else: false) else: let y = 2 in y) else: let z = 3 in z" "" "11111" ;
]

let two_dim_env_tests = [
    tr "tde_simple" "let a = 1 in let f1 = (lambda (x): x + a) in  f1(a)" "" "2";
    tr "tde_1" "let a = 1 in let f1 = (lambda (x): let b = 2 in let f2 = (lambda (z): b + z + a) in f2(b) + b) in f1(a) + a" "" "8"; 
    tr "tde_2" "let b = 99 in let f1 = (lambda (x): let bogus = 2 in b + x) in let a = 5 in f1(b) + a" "" "203"
  ]

let xtra_reg_alloc = [
  tr "rtt1" "let x = (1, 2, 3) in let y = (x, x, x) in let z = (1, 2) in y[1][1]" "" "2";
  tr "recursive_test_1" "let x = (1, 2, 3) in
                             let bogus = if true: begin x[0] := (7, 8, 9); 6 end else: 7 in 
                             let filler1 = (false, false, false) in let filler2 = 88 in
                             x[0][2]" "" "9" ;
  tr "linked_list_1" "(0, (1, (2, (3, (4, (5, nil))))))[1][1][0]" "" "2" ;
  tr "linked_list_2" "let rec gen_list = (lambda (n): if n == 0:
    nil
    else: let rest = gen_list(n - 1) in let a = 43 in (n, rest)) in
  let junk = (if true: (91, 92, 93, 94, 95, 96, 97, 98, 99)[7] else: true) in
  let l2 = gen_list(8) in
  l2[1][1][1][0]" "" "5"; 
  tr "linked_list_2_better" "let rec gen_list = (lambda (n): if n == 0:
    nil
    else: let rest = gen_list(n - 1) in (n, rest)) in
  let junkk = (if true: gen_list(5)[0]  else: false) in
  let l = gen_list(5) in
  l[1][1][1][0]" "" "2"                          
]

let regacc_tup_tests = [
  tr "ras_tup6" "((1, 2), (3, 4), (5, 6))" "" "((1, 2), (3, 4), (5, 6))";
  tr "ras_tup15" "let t = (1, 2, 3) in begin t[0] := (1, 2, 3); t[0] end" "" "(1, 2, 3)";
];;

let suite =
"unit_tests">:::
regacc_tup_tests @
xtra_reg_alloc @
recursive_gc_tests @
custom_oom_tests @
tons_of_garbage_tests @ 
basic_gc_tests @ 
two_dim_env_tests @ 
legacy @ 
fwd_ptr_tests @ 
simpler_tup_tests @
nullary_function @
small_tuple @
desugar_tests @
lib_tests @ 
is_well_formed_tests @ 
oom @ 
gc @
reg_acc_lambda @
two_dim_env_tests @
let_rec_tests @
simple_reg_acc @
reg_acc_many @
reg_acc_if

let () =
  run_test_tt_main ("all_tests">:::[suite; input_file_test_suite ()])
;;
