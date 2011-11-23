Control.Print.printDepth := 500000;
Control.Print.printLength := 500000;
Control.Print.stringDepth := 500000;
Control.polyEqWarn := false;

datatype expr = Var of string
              | Applic of expr * (expr list);
datatype inst = Push of string
              | Apply of int;

signature COMPILE_STACK_MACHINE =
sig
    val compile : expr -> inst list
    val pushl   : expr list -> inst list
end;


structure CompileStackMachine : COMPILE_STACK_MACHINE =
struct
    fun compile (Applic (e : expr, expr_list)) = pushl (rev expr_list) @ compile (e) @ [Apply (length (expr_list))]
      | compile (Var (v))                      = [Push (v)]

    and pushl ([])            = []   
      | pushl (exp1::exps)    = compile (exp1) @ pushl(exps)
end;

          (* Nothing like the smell of testing in the morning.. *)
(* CompileStackMachine.compile (Applic (Var "x", [])) =  [Push "x", Apply 0]; *)
(* CompileStackMachine.compile (Applic (Var "f", [Var "x"])) = [Push "x", Push "f", Apply 1]; *)
(* CompileStackMachine.compile (Applic (Var "f", [(Applic(Var "f", [Var "x"]))])) = [Push "x", Push "f", Apply 1, Push "f", Apply 1]; *)
(* CompileStackMachine.compile (Applic (Applic (Var "x", [Var "z"]), [Applic (Var "y", [Var "z"])])) = [Push "z", Push "y", Apply 1, Push "z", Push "x", Apply 1, Apply 1]; *)
(* CompileStackMachine.compile (Applic(Var("f"), [Var("1"), Var("2"), Var("3")])); *)
(* CompileStackMachine.compile (Applic(Var("g"), [Var("x"), Var("y")])); *)
(* CompileStackMachine.compile (Applic(Var("f"), [Applic(Var("g"), [Var("x"), Var("y")])])); *)
(* CompileStackMachine.compile (Applic(Var("f"), [Applic(Var("g"), [Var("x")]), Applic(Var("h"), [Applic(Var("h"), [Var("y")])]), Applic(Var("m"), [Applic(Var("m"), [Applic(Var("m"), [Var("z")])])])])) = [Push "z",Push "m",Apply 1,Push "m",Apply 1,Push "m",Apply 1,Push "y", *)
(*    Push "h",Apply 1,Push "h",Apply 1,Push "x",Push "g",Apply 1,Push "f",Apply 3]; *)

