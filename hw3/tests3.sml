SemanticAnalysis.analysis(TagParser.stringToPE "(x (lambda () (x (lambda () (x (lambda () (x (lambda () (x (x (x x)))))))))))") =
  AppTP
    (VarFree "x",
     [Abs
        ([],
         AppTP
           (VarFree "x",
            [Abs
               ([],
                AppTP
                  (VarFree "x",
                   [Abs
                      ([],
                       AppTP
                         (VarFree "x",
                          [Abs
                             ([],
                              AppTP
                                (VarFree "x",
                                 [App
                                    (VarFree "x",
                                     [App (VarFree "x",[VarFree "x"])])]))]))]))]))]);

SemanticAnalysis.analysis(TagParser.stringToPE "(set! x 3)") = Set (VarFree "x",Const (Number 3));

SemanticAnalysis.analysis(TagParser.stringToPE "(define x 3)") = Def (VarFree "x",Const (Number 3));

(print "an exception should be raised!!\n";
SemanticAnalysis.analysis(TagParser.stringToPE "#(a b c)"); false);

(print "an exception should be raised!!\n";
SemanticAnalysis.analysis(TagParser.stringToPE "()"); false);

SemanticAnalysis.analysis(TagParser.stringToPE "'()") = Const Nil;

SemanticAnalysis.analysis(TagParser.stringToPE "'#(a b c)") = Const (Vector [Symbol "a",Symbol "b",Symbol "c"]);

SemanticAnalysis.analysis(TagParser.stringToPE "''''a") =
  Const
    (Pair
       (Symbol "quote",
        Pair
          (Pair
             (Symbol "quote",
              Pair (Pair (Symbol "quote",Pair (Symbol "a",Nil)),Nil)),Nil)));

SemanticAnalysis.analysis(TagParser.stringToPE "'a") = Const (Symbol "a") ;

SemanticAnalysis.analysis(TagParser.stringToPE "(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))") =
  Def
    (VarFree "fact",
     Abs
       (["n"],
        If
          (App (VarFree "zero?",[VarParam ("n",0)]),Const (Number 1),
           AppTP
             (VarFree "*",
              [VarParam ("n",0),
               App
                 (VarFree "fact",
                  [App (VarFree "-",[VarParam ("n",0),Const (Number 1)])])]))));

