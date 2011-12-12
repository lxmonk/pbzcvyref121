(* Scanner.stringToTokens "()"; *)
(* print "0\n"; *)
(* Scanner.stringToTokens "(a b c)"; *)
(* print "1\n"; *)
(* Scanner.stringToTokens "(a . b)"; *)
(* print "2\n"; *)
(* Scanner.stringToTokens "(define abs (lambda (x) (if (negative? x) (- x) x)))"; *)
(* print "3\n"; *)
(* Scanner.stringToTokens "#\\A"; *)
(* print "4\n"; *)
(* Scanner.stringToTokens "#\\space"; *)
(* print "5\n"; *)
(* Scanner.stringToTokens "#\\return"; *)
(* print "6\n"; *)
(* Scanner.stringToTokens "#\\newline"; *)
(* print "7\n"; *)
(* Scanner.stringToTokens "#\\tab"; *)
(* print "8\n"; *)
(* Scanner.stringToTokens ")(."; *)
(* print "9\n"; *)
(* Scanner.stringToTokens "#(a b c)"; *)
(* print "10\n"; *)
(* Scanner.stringToTokens "#(a b . c)"; *)
(* print "11\n"; *)
(* Reader.stringToSexpr "(a)"; *)
(* print "12\n"; *)
(* Reader.stringToSexpr "(a b . c)"; *)
(* print "13\n"; *)
(* Reader.stringToSexpr "(define abs (lambda (x) (if (negative? x) (- x) x)))"; *)
(* print "14\n"; *)
(* Reader.stringToSexpr "#(a b c)"; *)
(* print "15\n"; *)
(* Reader.stringToSexpr "()"; *)
(* print "16\n"; *)
(* TagParser.stringToPE "x"; *)
(* print "17\n"; *)
(* TagParser.stringToPE "'x"; *)
(* print "18\n"; *)
(* TagParser.stringToPE "()"; *)
(* print "19\n"; *)
(* TagParser.stringToPE "'()"; *)
(* print "20\n"; *)
(* TagParser.stringToPE "#(1 2 3)"; *)
(* print "21\n"; *)
(* TagParser.stringToPE "'#(1 2 3)"; *)
(* print "22\n"; *)
(* TagParser.stringToPE "496351"; *)
(* print "23\n"; *)
(* TagParser.stringToPE "'496351"; *)
(* print "24\n"; *)
(* TagParser.stringToPE "''496351"; *)
(* print "25\n"; *)
(* TagParser.stringToPE "'''496351"; *)
(* print "26\n"; *)
(* TagParser.stringToPE "(define abs (lambda (x) (if (negative? x) (- x) x)))"; *)
(* print "27\n"; *)
(* (* TagParser.stringToPE "lambda"; *) *)
(* (* print "28\n"; *) *)
(* (* TagParser.stringToPE "define"; *) *)
(* (* print "29\n"; *) *)
(* (* TagParser.stringToPE "quote"; *) *)
(* (* print "30\n"; *) *)
(* TagParser.stringToPE "(lambda x x)"; *)
(* print "31\n"; *)
(* TagParser.stringToPE "(lambda (x) x)"; *)
(* print "32\n"; *)
(* TagParser.stringToPE "(lambda (x . rest) x)"; *)
(* print "33\n"; *)
(* TagParser.stringToPE "(begin (display \"foo\") (+ 2 3))"; *)
(* print "34\n"; *)
(* TagParser.stringToPE "(begin (display \"foo\"))"; *)
(* print "35\n"; *)
(* TagParser.stringToPE "(begin)"; *)
(* print "36\n"; *)
(* TagParser.stringToPE "(set! abc 234)"; *)
(* print "37\n"; *)
(* TagParser.stringToPE "(define a 3)"; *)
(* print "38\n"; *)
(* TagParser.stringToPE "(define (a) 3)"; *)
(* print "39\n"; *)
(* TagParser.stringToPE "(define (a b) 3)"; *)
(* print "40\n"; *)
(* TagParser.stringToPE "(define (a b . c) 3)"; *)
(* print "41\n"; *)
(* TagParser.stringToPE "(define (a . c) 3)"; *)
(* print "42\n"; *)
(* TagParser.stringToPE "(if E1 E2 E3)"; *)
(* print "43\n"; *)
(* TagParser.stringToPE "(if E1 E2 (if E3 E4 E5))"; *)
(* print "44\n"; *)
(* TagParser.stringToPE "(if E1 E2)"; *)
(* print "45\n"; *)
(* TagParser.stringToPE "(cond ((positive? a) 'positive) ((negative? a) 'negative) (else 'zero))"; *)
(* print "46\n"; *)
(* TagParser.stringToPE "(let ((a 3) (b 5)) (+ a b))"; *)
(* print "47\n"; *)
(* TagParser.stringToPE "(let* ((a 3) (b 5)) (+ a b))"; *)
(* print "48\n"; *)
(* TagParser.stringToPE "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (+ (fact 2) (fact 5)))"; *)
(* print "49\n"; *)
(* TagParser.stringToPE "(letrec ((even? (lambda (n) (if (zero? n) #t (odd? - n 1)))) (odd? (lambda (n) (if (zero? n) #f) (even? (- n 1))))) (and (even? x) (odd? y)))"; *)
(* print "50\n"; *)
(* TagParser.stringToPE "(define foo (lambda (x y) (define even? (lambda (n) (if (zero? n) #t (odd? - n 1)))) (define odd? (lambda (n) (if (zero? n) #f) (even? (- n 1)))) (and (even? x) (odd? y))))"; *)
(* print "51\n"; *)
(* TagParser.stringToPEs "(a b)"; *)
(* print "52\n"; *)
(* TagParser.stringToPEs "(define abs (lambda (x) (if (negative? x) (- x) x))) (+ (abs x) (abs y))"; *)
(* print "53\n"; *)
(* TagParser.stringToPEs "(and E1 E2) (or E1 E2 E3)"; *)
(* print "54\n"; *)
(* Reader.stringToSexpr "'(a . b)"; *)
(* print "55\n"; *)
(* Reader.stringToSexpr "(quote (a . b))"; *)
(* print "56\n"; *)
(* Reader.stringToSexpr "(a . (b . (c . ())))"; *)
(* print "57\n"; *)
(* Reader.stringToSexpr "(a b c)"; *)
(* print "58\n"; *)
(* Reader.stringToSexpr "(((a) . b) . c)"; *)
(* print "59\n"; *)
(* Reader.stringToSexpr "\"abc\""; *)
(* print "60\n"; *)
(* Reader.stringToSexpr "'\"abc\""; *)
(* print "61\n"; *)
(* TagParser.stringToPE "\"abc\""; *)
(* print "62\n"; *)
(* TagParser.stringToPE "'\"abc\""; *)
(* print "63\n"; *)
(* TagParser.stringToPE "(quote \"abc\")"; *)
(* print "64\n"; *)
(* Reader.stringToSexpr "#\\t"; *)
(* print "65\n"; *)
(* Reader.stringToSexpr "#t"; *)
(* print "66\n"; *)
(* (* Reader.stringToSexpr "#(1 2 3 . 5)"; *) *)
(* (* print "67\n"; *) *)
(* Reader.stringToSexpr "#(1 2 3 5)"; *)
(* print "68\n"; *)
(* Reader.stringToSexpr "#(() #() #(1 2) #\\A #\\newline (3 . a))"; *)
(* print "69\n"; *)
*)

Scanner.stringToTokens "()" = [LparenToken,RparenToken] ;
print "0";
 Scanner.stringToTokens "(a b c)" =
  [LparenToken,SymbolToken "a",SymbolToken "b",SymbolToken "c",RparenToken]
  ;
print "1";
 Scanner.stringToTokens "(a . b)" = [LparenToken,SymbolToken "a",DotToken,SymbolToken "b",RparenToken]
  ;
print "2";
 Scanner.stringToTokens "(define abs (lambda (x) (if (negative? x) (- x) x)))" =
  [LparenToken,SymbolToken "define",SymbolToken "abs",LparenToken,
   SymbolToken "lambda",LparenToken,SymbolToken "x",RparenToken,LparenToken,
   SymbolToken "if",LparenToken,SymbolToken "negative?",SymbolToken "x",
   RparenToken,LparenToken,SymbolToken "-",SymbolToken "x",RparenToken,
   SymbolToken "x",RparenToken,RparenToken,RparenToken] ;
print "3";
 Scanner.stringToTokens "#\\A" = [CharToken #"A"] ;
print "4";
 Scanner.stringToTokens "#\\space" = [CharToken #" "] ;
print "5";
 Scanner.stringToTokens "#\\return" = [CharToken #"\r"] ;
print "6";
 Scanner.stringToTokens "#\\newline" = [CharToken #"\n"] ;
print "7";
 Scanner.stringToTokens "#\\tab" = [CharToken #"\t"] ;
print "8";
 Scanner.stringToTokens ")(." = [RparenToken,LparenToken,DotToken] ;
print "9";
 Scanner.stringToTokens "#(a b c)" =
  [VectorToken,SymbolToken "a",SymbolToken "b",SymbolToken "c",RparenToken]
  ;
print "10";
 Scanner.stringToTokens "#(a b . c)" =
  [VectorToken,SymbolToken "a",SymbolToken "b",DotToken,SymbolToken "c",
   RparenToken] ;
print "11";
 Reader.stringToSexpr "(a)" = Pair (Symbol "a",Nil) ;
print "12";
 Reader.stringToSexpr "(a b . c)" = Pair (Symbol "a",Pair (Symbol "b",Symbol "c")) ;
print "13";
 Reader.stringToSexpr "(define abs (lambda (x) (if (negative? x) (- x) x)))" =
  Pair
    (Symbol "define",
     Pair
       (Symbol "abs",
        Pair
          (Pair
             (Symbol "lambda",
              Pair
                (Pair (Symbol "x",Nil),
                 Pair
                   (Pair
                      (Symbol "if",
                       Pair
                         (Pair (Symbol "negative?",Pair (Symbol "x",Nil)),
                          Pair
                            (Pair (Symbol "-",Pair (Symbol "x",Nil)),
                             Pair (Symbol "x",Nil)))),Nil))),Nil))) ;
print "14";
 Reader.stringToSexpr "#(a b c)" = Vector [Symbol "a",Symbol "b",Symbol "c"] ;
print "15";
 Reader.stringToSexpr "()" = Nil ;
print "16";
 TagParser.stringToPE "x" = Var "x" ;
print "17";
 TagParser.stringToPE "'x" = Const (Symbol "x") ;
print "18";
 TagParser.stringToPE "()" = Const Nil ;
print "19";
 TagParser.stringToPE "'()" = Const Nil ;
print "20";
 TagParser.stringToPE "#(1 2 3)" = Const (Vector [Number 1,Number 2,Number 3]) ;
print "21";
 TagParser.stringToPE "'#(1 2 3)" = Const (Vector [Number 1,Number 2,Number 3]) ;
print "22";
 TagParser.stringToPE "496351" = Const (Number 496351) ;
print "23";
 TagParser.stringToPE "'496351" = Const (Number 496351) ;
print "24";
 TagParser.stringToPE "''496351" = Const (Pair (Symbol "quote",Pair (Number 496351,Nil))) ;
print "25";
 TagParser.stringToPE "'''496351" =
  Const
    (Pair
       (Symbol "quote",
        Pair (Pair (Symbol "quote",Pair (Number 496351,Nil)),Nil))) ;
print "26";
 TagParser.stringToPE "(define abs (lambda (x) (if (negative? x) (- x) x)))" =
  Def
    (Var "abs",
     Abs
       (["x"],
        If (App (Var "negative?",[Var "x"]),App (Var "-",[Var "x"]),Var "x")))
  ;
print "27";
(*  TagParser.stringToPE "lambda"; *)

(* uncaught exception ErrorReservedWordUsedImproperly *)
(*   raised at: compiler.sml; *)
(* print "28"; *)
(*  TagParser.stringToPE "define"; *)

(* uncaught exception ErrorReservedWordUsedImproperly *)
(*   raised at: compiler.sml; *)
(* print "29"; *)
(*  TagParser.stringToPE "quote"; *)

(* uncaught exception ErrorReservedWordUsedImproperly *)
(*   raised at: compiler.sml; *)
print "30";
 TagParser.stringToPE "(lambda x x)" = AbsVar ("x",Var "x") ;
print "31";
 TagParser.stringToPE "(lambda (x) x)" = Abs (["x"],Var "x") ;
print "32";
 TagParser.stringToPE "(lambda (x . rest) x)" = AbsOpt (["x"],"rest",Var "x") ;
print "33";
 TagParser.stringToPE "(begin (display \"foo\") (+ 2 3))" =
  Seq
    [App (Var "display",[Const (String "foo")]),
     App (Var "+",[Const (Number 2),Const (Number 3)])] ;
print "34";
 TagParser.stringToPE "(begin (display \"foo\"))" = App (Var "display",[Const (String "foo")]) ;
print "35";
 TagParser.stringToPE "(begin)" = Const Void ;
print "36";
 TagParser.stringToPE "(set! abc 234)" = Set (Var "abc",Const (Number 234)) ;
print "37";
 TagParser.stringToPE "(define a 3)" = Def (Var "a",Const (Number 3)) ;
print "38";
 TagParser.stringToPE "(define (a) 3)" = Def (Var "a",Abs ([],Const (Number 3))) ;
print "39";
 TagParser.stringToPE "(define (a b) 3)" = Def (Var "a",Abs (["b"],Const (Number 3))) ;
print "40";
 TagParser.stringToPE "(define (a b . c) 3)" = Def (Var "a",AbsOpt (["b"],"c",Const (Number 3))) ;
print "41";
 TagParser.stringToPE "(define (a . c) 3)" = Def (Var "a",AbsVar ("c",Const (Number 3))) ;
print "42";
 TagParser.stringToPE "(if E1 E2 E3)" = If (Var "E1",Var "E2",Var "E3") ;
print "43";
 TagParser.stringToPE "(if E1 E2 (if E3 E4 E5))" = If (Var "E1",Var "E2",If (Var "E3",Var "E4",Var "E5")) ;
print "44";
 TagParser.stringToPE "(if E1 E2)" = If (Var "E1",Var "E2",Const Void) ;
print "45";
 TagParser.stringToPE "(cond ((positive? a) 'positive) ((negative? a) 'negative) (else 'zero))" =
  If
    (App (Var "positive?",[Var "a"]),Const (Symbol "positive"),
     If
       (App (Var "negative?",[Var "a"]),Const (Symbol "negative"),
        Const (Symbol "zero"))) ;
print "46";
 TagParser.stringToPE "(let ((a 3) (b 5)) (+ a b))" =
  App
    (Abs (["a","b"],App (Var "+",[Var "a",Var "b"])),
     [Const (Number 3),Const (Number 5)]) ;
print "47";
 TagParser.stringToPE "(let* ((a 3) (b 5)) (+ a b))" =
  App
    (Abs
       (["a"],
        App (Abs (["b"],App (Var "+",[Var "a",Var "b"])),[Const (Number 5)])),
     [Const (Number 3)]) ;
print "48";
 TagParser.stringToPE "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (+ (fact 2) (fact 5)))" =
  App
    (Abs
       (["fact"],
        Seq
          [Set
             (Var "fact",
              Abs
                (["n"],
                 If
                   (App (Var "zero?",[Var "n"]),Const (Number 1),
                    App
                      (Var "*",
                       [Var "n",
                        App
                          (Var "fact",
                           [App (Var "-",[Var "n",Const (Number 1)])])])))),
           App
             (Abs
                ([],
                 App
                   (Var "+",
                    [App (Var "fact",[Const (Number 2)]),
                     App (Var "fact",[Const (Number 5)])])),[])]),
     [Const Void]) ;
print "49";
 TagParser.stringToPE "(letrec ((even? (lambda (n) (if (zero? n) #t (odd? - n 1)))) (odd? (lambda (n) (if (zero? n) #f) (even? (- n 1))))) (and (even? x) (odd? y)))" =
  App
    (Abs
       (["even?","odd?"],
        Seq
          [Seq
             [Set
                (Var "even?",
                 Abs
                   (["n"],
                    If
                      (App (Var "zero?",[Var "n"]),Const (Bool true),
                       App (Var "odd?",[Var "-",Var "n",Const (Number 1)])))),
              Set
                (Var "odd?",
                 Abs
                   (["n"],
                    Seq
                      [If
                         (App (Var "zero?",[Var "n"]),Const (Bool false),
                          Const Void),
                       App
                         (Var "even?",
                          [App (Var "-",[Var "n",Const (Number 1)])])]))],
           App
             (Abs
                ([],
                 If
                   (App (Var "even?",[Var "x"]),App (Var "odd?",[Var "y"]),
                    Const (Bool false))),[])]),[Const Void,Const Void]) ;
 print "50";
 (* TagParser.stringToPE "(define foo (lambda (x y) (define even? (lambda (n) (if (zero? n) #t (odd? - n 1)))) (define odd? (lambda (n) (if (zero? n) #f) (even? (- n 1)))) (and (even? x) (odd? y))))" = *)
 (*  Def *)
 (*    (Var "foo", *)
 (*     Abs *)
 (*       (["x","y"], *)
 (*        App *)
 (*          (Abs *)
 (*             (["even?","odd?"], *)
 (*              Seq *)
 (*                [Seq *)
 (*                   [Set *)
 (*                      (Var "even?", *)
 (*                       Abs *)
 (*                         (["n"], *)
 (*                          If *)
 (*                            (App (Var "zero?",[Var "n"]),Const (Bool true), *)
 (*                             App *)
 (*                               (Var "odd?",[Var "-",Var "n",Const (Number 1)])))), *)
 (*                    Set *)
 (*                      (Var "odd?", *)
 (*                       Abs *)
 (*                         (["n"], *)
 (*                          Seq *)
 (*                            [If *)
 (*                               (App (Var "zero?",[Var "n"]), *)
 (*                                Const (Bool false),Const Void), *)
 (*                             App *)
 (*                               (Var "even?", *)
 (*                                [App (Var "-",[Var "n",Const (Number 1)])])]))], *)
 (*                 App *)
 (*                   (Abs *)
 (*                      ([], *)
 (*                       If *)
 (*                         (App (Var "even?",[Var "x"]), *)
 (*                          App (Var "odd?",[Var "y"]),Const (Bool false))),[])]), *)
 (*           [Const Void,Const Void]))) ; *)
print "51";
 TagParser.stringToPEs "(a b)" = [App (Var "a",[Var "b"])] ;
print "52";
 TagParser.stringToPEs "(define abs (lambda (x) (if (negative? x) (- x) x))) (+ (abs x) (abs y))" =
  [Def
     (Var "abs",
      Abs
        (["x"],
         If (App (Var "negative?",[Var "x"]),App (Var "-",[Var "x"]),Var "x"))),
   App (Var "+",[App (Var "abs",[Var "x"]),App (Var "abs",[Var "y"])])]
  ;
print "53";
 TagParser.stringToPEs "(and E1 E2) (or E1 E2 E3)" =
  [If (Var "E1",Var "E2",Const (Bool false)),Or [Var "E1",Var "E2",Var "E3"]]
  ;
print "54";
 Reader.stringToSexpr "'(a . b)" = Pair (Symbol "quote",Pair (Pair (Symbol "a",Symbol "b"),Nil)) ;
print "55";
 Reader.stringToSexpr "(quote (a . b))" = Pair (Symbol "quote",Pair (Pair (Symbol "a",Symbol "b"),Nil)) ;
print "56";
 Reader.stringToSexpr "(a . (b . (c . ())))" = Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Nil))) ;
print "57";
 Reader.stringToSexpr "(a b c)" = Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Nil))) ;
print "58";
 Reader.stringToSexpr "(((a) . b) . c)" = Pair (Pair (Pair (Symbol "a",Nil),Symbol "b"),Symbol "c") ;
print "59";
 Reader.stringToSexpr "\"abc\"" = String "abc" ;
print "60";
 Reader.stringToSexpr "'\"abc\"" = Pair (Symbol "quote",Pair (String "abc",Nil)) ;
print "61";
 TagParser.stringToPE "\"abc\"" = Const (String "abc") ;
print "62";
 TagParser.stringToPE "'\"abc\"" = Const (String "abc") ;
print "63";
 TagParser.stringToPE "(quote \"abc\")" = Const (String "abc") ;
print "64";
 Reader.stringToSexpr "#\\t" = Char #"t" ;
print "65";
 Reader.stringToSexpr "#t" = Bool true ;
print "66";
(*  Reader.stringToSexpr "#(1 2 3 . 5)"; *)

(* uncaught exception ErrorNoClosingRparen *)
(*   raised at: compiler.sml; *)
print "67";
 Reader.stringToSexpr "#(1 2 3 5)" = Vector [Number 1,Number 2,Number 3,Number 5] ;
print "68";
 Reader.stringToSexpr "#(() #() #(1 2) #\\A #\\newline (3 . a))" =
  Vector
    [Nil,Vector [],Vector [Number 1,Number 2],Char #"A",Char #"\n",
     Pair (Number 3,Symbol "a")] ;
print "69";
