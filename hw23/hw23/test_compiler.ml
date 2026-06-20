(* test_compiler.ml *)

(* Load the compiler module *)
#use "compiler.ml";;

(* Helper function to compare expr types *)
let test_parse name input expected =
  try
    let actual = parse input in
    if actual = expected then
      Printf.printf "PASS: %s\n" name
    else begin
      Printf.printf "FAIL: %s\nExpected: %s\nGot: %s\n\n" 
        name 
        (string_of_expr expected) 
        (string_of_expr actual)
    end
  with
  | e -> Printf.printf "ERROR: %s\nException: %s\n\n" name (Printexc.to_string e)

(* Helper function to compare expr' types *)
let test_sem name input expected =
  try
    let actual = Semantic_Analysis.semantics (parse input) in
    if actual = expected then
      Printf.printf "PASS: %s\n" name
    else begin
      Printf.printf "FAIL: %s\nExpected: %s\nGot: %s\n\n" 
        name 
        (string_of_expr' expected) 
        (string_of_expr' actual)
    end
  with
  | e -> Printf.printf "ERROR: %s\nException: %s\n\n" name (Printexc.to_string e)

(* Define the parse tests *)
let run_parse_tests () =
  Printf.printf "Running parse tests...\n\n";
  
  (* Existing Parse Tests *)
  
  (* Test Case: parse "496351" *)
  test_parse "parse_integer" "496351" (ScmConst (ScmNumber (ScmInteger 496351)));
  
  (* Test Case: parse "a" *)
  test_parse "parse_variable" "a" (ScmVarGet (Var "a"));
  
  (* Test Case: parse " 'a " *)
  test_parse "parse_symbol" " 'a " (ScmConst (ScmSymbol "a"));
  
  (* Test Case: parse " '234 " *)
  test_parse "parse_symbol_number" " '234 " (ScmConst (ScmNumber (ScmInteger 234)));
  
  (* Test Case: parse " (if a b c) " *)
  test_parse "parse_if_simple" " (if a b c) " 
    (ScmIf (ScmVarGet (Var "a"), ScmVarGet (Var "b"), ScmVarGet (Var "c")));
  
  (* Test Case: parse complex if expression *)
  test_parse "parse_if_nested" " (if (< a b) (if (< a c) b c) (+ a b c)) " 
    (ScmIf
      (ScmApplic (ScmVarGet (Var "<"), [ScmVarGet (Var "a"); ScmVarGet (Var "b")]),
       ScmIf (ScmApplic (ScmVarGet (Var "<"), [ScmVarGet (Var "a"); ScmVarGet (Var "c")]),
              ScmVarGet (Var "b"),
              ScmVarGet (Var "c")),
       ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b"); ScmVarGet (Var "c")])
      )
    );
  
  (* Test Case: parse " (set! a 3) " *)
  test_parse "parse_set!" " (set! a 3) " 
    (ScmVarSet (Var "a", ScmConst (ScmNumber (ScmInteger 3))));
  
  (* Test Case: parse " (define a 3) " *)
  test_parse "parse_define" " (define a 3) " 
    (ScmVarDef (Var "a", ScmConst (ScmNumber (ScmInteger 3))));
  
  (* Test Case: parse " (begin (a b) (c d) (e f)) " *)
  test_parse "parse_begin" " (begin (a b) (c d) (e f)) " 
    (ScmSeq [
      ScmApplic (ScmVarGet (Var "a"), [ScmVarGet (Var "b")]);
      ScmApplic (ScmVarGet (Var "c"), [ScmVarGet (Var "d")]);
      ScmApplic (ScmVarGet (Var "e"), [ScmVarGet (Var "f")])
    ]);
  
  (* Test Case: parse " (or (a b) (c d) (e f)) " *)
  test_parse "parse_or" " (or (a b) (c d) (e f)) " 
    (ScmOr [
      ScmApplic (ScmVarGet (Var "a"), [ScmVarGet (Var "b")]);
      ScmApplic (ScmVarGet (Var "c"), [ScmVarGet (Var "d")]);
      ScmApplic (ScmVarGet (Var "e"), [ScmVarGet (Var "f")])
    ]);
  
  (* Test Case: parse " (lambda (a b c) (+ a b c)) " *)
  test_parse "parse_lambda_simple" " (lambda (a b c) (+ a b c)) " 
    (ScmLambda (["a"; "b"; "c"], Simple,
      ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b"); ScmVarGet (Var "c")])
    ));
  
  (* Test Case: parse " (lambda (a b . c) (list a b c)) " *)
  test_parse "parse_lambda_opt" " (lambda (a b . c) (list a b c)) " 
    (ScmLambda (["a"; "b"], Opt "c",
      ScmApplic (ScmVarGet (Var "list"), [ScmVarGet (Var "a"); ScmVarGet (Var "b"); ScmVarGet (Var "c")])
    ));
  
  (* Test Case: parse " (lambda a (list a b c)) " *)
  test_parse "parse_lambda_opt_only" " (lambda a (list a b c)) " 
    (ScmLambda ([], Opt "a",
      ScmApplic (ScmVarGet (Var "list"), [ScmVarGet (Var "a"); ScmVarGet (Var "b"); ScmVarGet (Var "c")])
    ));
  
  (* Test Case: parse " (lambda () (list a b c)) " *)
  test_parse "parse_lambda_no_args" " (lambda () (list a b c)) " 
    (ScmLambda ([], Simple,
      ScmApplic (ScmVarGet (Var "list"), [ScmVarGet (Var "a"); ScmVarGet (Var "b"); ScmVarGet (Var "c")])
    ));
  
  (* Test Case: parse " `(a ,b ,@c d) " *)
  test_parse "parse_application_with_splicing" " `(a ,b ,@c d) " 
    (ScmApplic (ScmVarGet (Var "cons"),
      [
        ScmConst (ScmSymbol "a");
        ScmApplic (ScmVarGet (Var "cons"),
          [
            ScmVarGet (Var "b");
            ScmApplic (ScmVarGet (Var "append"),
              [
                ScmVarGet (Var "c");
                ScmApplic (ScmVarGet (Var "cons"),
                  [
                    ScmConst (ScmSymbol "d");
                    ScmConst ScmNil
                  ])
              ])
          ])
      ]
    ));
  
  (* Test Case: parse complex cond expression *)
  test_parse "parse_cond" "
    (cond ((f? x) (f y) (f z))
          ((g? x) (g y) (g z))
          (else '()))
  " 
    (ScmIf (
      ScmApplic (ScmVarGet (Var "f?"), [ScmVarGet (Var "x")]),
      ScmSeq [
        ScmApplic (ScmVarGet (Var "f"), [ScmVarGet (Var "y")]);
        ScmApplic (ScmVarGet (Var "f"), [ScmVarGet (Var "z")])
      ],
      ScmIf (
        ScmApplic (ScmVarGet (Var "g?"), [ScmVarGet (Var "x")]),
        ScmSeq [
          ScmApplic (ScmVarGet (Var "g"), [ScmVarGet (Var "y")]);
          ScmApplic (ScmVarGet (Var "g"), [ScmVarGet (Var "z")])
        ],
        ScmConst ScmNil
      )
    ));
  
  (* Test Case: parse " (if (zero? x) (display \"just lovely!\") ) " *)
  test_parse "parse_if_with_display" " (if (zero? x) (display \"just lovely!\")) " 
    (ScmIf (
      ScmApplic (ScmVarGet (Var "zero?"), [ScmVarGet (Var "x")]),
      ScmApplic (ScmVarGet (Var "display"), [ScmConst (ScmString "just lovely!")]),
      ScmConst ScmVoid
    ));
  
  (* *** Added Missing Parse Tests *** *)
  
  (* Test Case: parse " (let () (+ a b)) " *)
  test_parse "parse_let_no_bindings" " (let () (+ a b)) " 
    (ScmApplic
      (ScmLambda ([], Simple,
        ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b")])
      ), [])
    );
  
  (* Test Case: parse " (let ((a 2) (b 3)) (+ a b)) " *)
  test_parse "parse_let_with_bindings" " (let ((a 2) (b 3)) (+ a b)) " 
    (ScmApplic
      (ScmLambda (["a"; "b"], Simple,
        ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b")])
      ), [ScmConst (ScmNumber (ScmInteger 2)); ScmConst (ScmNumber (ScmInteger 3))])
    );
  
  (* Test Case: parse " (let* () (+ a b)) " *)
  test_parse "parse_let_star_no_bindings" " (let* () (+ a b)) " 
    (ScmApplic
      (ScmLambda ([], Simple,
        ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b")])
      ), [])
    );
  
  (* Test Case: parse " (let* ((a 2) (b 3)) (+ a b)) " *)
  test_parse "parse_let_star_with_bindings" " (let* ((a 2) (b 3)) (+ a b)) " 
    (ScmApplic
      (ScmLambda (["a"], Simple,
        ScmApplic
          (ScmLambda (["b"], Simple,
            ScmApplic (ScmVarGet (Var "+"), [ScmVarGet (Var "a"); ScmVarGet (Var "b")])
          ), [ScmConst (ScmNumber (ScmInteger 3))])
      ), [ScmConst (ScmNumber (ScmInteger 2))])
    );
  
  (* Test Case: parse " (letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (+ (fact 5) (fact 55))) " *)
  test_parse "parse_letrec_fact" " (letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (+ (fact 5) (fact 55))) " 
    (ScmApplic
      (ScmLambda (["fact"], Simple,
        ScmSeq
          [
            ScmVarSet (Var "fact", ScmLambda (["n"], Simple,
              ScmIf (ScmApplic (ScmVarGet (Var "zero?"), [ScmVarGet (Var "n")]),
                     ScmConst (ScmNumber (ScmInteger 1)),
                     ScmApplic (ScmVarGet (Var "*"),
                                [ScmVarGet (Var "n");
                                 ScmApplic (ScmVarGet (Var "fact"),
                                            [ScmApplic (ScmVarGet (Var "-"),
                                                        [ScmVarGet (Var "n"); ScmConst (ScmNumber (ScmInteger 1))])])])
              ))
            );
            ScmApplic (ScmVarGet (Var "+"),
                       [
                         ScmApplic (ScmVarGet (Var "fact"), [ScmConst (ScmNumber (ScmInteger 5))]);
                         ScmApplic (ScmVarGet (Var "fact"), [ScmConst (ScmNumber (ScmInteger 55))])
                       ])
          ]
      ), [ScmConst (ScmSymbol "whatever")]
      )
    );

;;

(* Define the sem tests *)
let run_sem_tests () =
  Printf.printf "\nRunning semantic analysis (sem) tests...\n\n";
  
  (* Existing Semantic Analysis Tests *)
  
  (* Test Case: sem "234" *)
  test_sem "sem_integer" " 234 " (ScmConst' (ScmNumber (ScmInteger 234)));
  
  (* Test Case: sem "a" *)
  test_sem "sem_variable" " a " (ScmVarGet' (Var' ("a", Free)));
  
  (* Test Case: sem " 'a " *)
  test_sem "sem_symbol" " 'a " (ScmConst' (ScmSymbol "a"));
  
  (* Test Case: sem " ''a " *)
  test_sem "sem_quote_symbol" " ''a " (ScmConst' (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil))));
  
  (* Test Case: sem " (+ a b c) " *)
  test_sem "sem_application_plus" " (+ a b c) " 
    (ScmApplic' (
      ScmVarGet' (Var' ("+", Free)),
      [
        ScmVarGet' (Var' ("a", Free));
        ScmVarGet' (Var' ("b", Free));
        ScmVarGet' (Var' ("c", Free))
      ],
      Non_Tail_Call
    ));
  
  (* Test Case: sem " (lambda (a) (lambda (b c) (+ a b c))) " *)
  test_sem "sem_lambda_nested" " (lambda (a) (lambda (b c) (+ a b c))) " 
    (ScmLambda' (
      ["a"], Simple,
      ScmLambda' (
        ["b"; "c"], Simple,
        ScmApplic' (
          ScmVarGet' (Var' ("+", Free)),
          [
            ScmVarGet' (Var' ("a", Bound (0, 0)));
            ScmVarGet' (Var' ("b", Param 0));
            ScmVarGet' (Var' ("c", Param 1))
          ],
          Tail_Call
        )
      )
    ));
  
  (* Test Case: sem " (lambda (a) (lambda (b c) (+ (* a a) (* b b) (* c c)))) " *)
  test_sem "sem_lambda_with_operations" "
    (lambda (a) (lambda (b c) (+ (* a a) (* b b) (* c c))))
  " 
    (ScmLambda' (
      ["a"], Simple,
      ScmLambda' (
        ["b"; "c"], Simple,
        ScmApplic' (
          ScmVarGet' (Var' ("+", Free)),
          [
            ScmApplic' (ScmVarGet' (Var' ("*", Free)), [ScmVarGet' (Var' ("a", Bound (0, 0))); ScmVarGet' (Var' ("a", Bound (0, 0)))], Non_Tail_Call);
            ScmApplic' (ScmVarGet' (Var' ("*", Free)), [ScmVarGet' (Var' ("b", Param 0)); ScmVarGet' (Var' ("b", Param 0))], Non_Tail_Call);
            ScmApplic' (ScmVarGet' (Var' ("*", Free)), [ScmVarGet' (Var' ("c", Param 1)); ScmVarGet' (Var' ("c", Param 1))], Non_Tail_Call)
          ],
          Tail_Call
        )
      )
    ));
  
  (* Test Case: sem " (let ((a 0)) (list (lambda () a) (lambda (v) (set! a v))) ) " *)
  test_sem "sem_let_with_lambda_and_box" "
    (let ((a 0))
      (list (lambda () a)
            (lambda (v) (set! a v))))
  " 
    (ScmApplic' (
      ScmLambda' (
        ["a"], Simple,
        ScmSeq' [
          ScmVarSet' (Var' ("a", Param 0), ScmBox' (Var' ("a", Param 0)));
          ScmApplic' (
            ScmVarGet' (Var' ("list", Free)),
            [
              ScmLambda' ([], Simple, ScmBoxGet' (Var' ("a", Bound (0, 0))));
              ScmLambda' (["v"], Simple, ScmBoxSet' (Var' ("a", Bound (0, 0)), ScmVarGet' (Var' ("v", Param 0))))
            ],
            Tail_Call
          )
        ]
      ),
      [
        ScmConst' (ScmNumber (ScmInteger 0))
      ],
      Non_Tail_Call
    ));
  
  (* Test Case: sem " (+ (let () (* a a)) (let () (* b b))) " *)
  test_sem "sem_application_with_let" "
    (+ (let () (* a a)) (let () (* b b)))
  " 
    (ScmApplic' (
      ScmVarGet' (Var' ("+", Free)),
      [
        ScmApplic' (
          ScmLambda' ([], Simple,
            ScmApplic' (ScmVarGet' (Var' ("*", Free)), [ScmVarGet' (Var' ("a", Free)); ScmVarGet' (Var' ("a", Free))], Tail_Call)
          ),
          [],
          Non_Tail_Call
        );
        ScmApplic' (
          ScmLambda' ([], Simple,
            ScmApplic' (ScmVarGet' (Var' ("*", Free)), [ScmVarGet' (Var' ("b", Free)); ScmVarGet' (Var' ("b", Free))], Tail_Call)
          ),
          [],
          Non_Tail_Call
        )
      ],
      Non_Tail_Call
    ));
  
  (* Test Case: sem " (cond ((f? x) (f y) (f z)) ((g? x) (g y) (g z)) (else '())) " *)
  test_sem "sem_cond" "
    (cond ((f? x) (f y) (f z))
          ((g? x) (g y) (g z))
          (else '()))
  " 
    (ScmIf' (
      ScmApplic' (ScmVarGet' (Var' ("f?", Free)), [ScmVarGet' (Var' ("x", Free))], Non_Tail_Call),
      ScmSeq' [
        ScmApplic' (ScmVarGet' (Var' ("f", Free)), [ScmVarGet' (Var' ("y", Free))], Non_Tail_Call);
        ScmApplic' (ScmVarGet' (Var' ("f", Free)), [ScmVarGet' (Var' ("z", Free))], Non_Tail_Call)
      ],
      ScmIf' (
        ScmApplic' (ScmVarGet' (Var' ("g?", Free)), [ScmVarGet' (Var' ("x", Free))], Non_Tail_Call),
        ScmSeq' [
          ScmApplic' (ScmVarGet' (Var' ("g", Free)), [ScmVarGet' (Var' ("y", Free))], Non_Tail_Call);
          ScmApplic' (ScmVarGet' (Var' ("g", Free)), [ScmVarGet' (Var' ("z", Free))], Non_Tail_Call)
        ],
        ScmConst' ScmNil
      )
    ));
  
  (* Test Case: sem " (if (zero? x) (display \"just lovely!\") ) " *)
  test_sem "sem_if_with_display" "
    (if (zero? x) (display \"just lovely!\"))
  " 
    (ScmIf' (
      ScmApplic' (ScmVarGet' (Var' ("zero?", Free)), [ScmVarGet' (Var' ("x", Free))], Non_Tail_Call),
      ScmApplic' (ScmVarGet' (Var' ("display", Free)), [ScmConst' (ScmString "just lovely!")], Non_Tail_Call),
      ScmConst' ScmVoid
    ));
;;

(* Run all tests *)
let () =
  run_parse_tests ();
  run_sem_tests ();
  Printf.printf "\nAll tests completed.\n"
;;
