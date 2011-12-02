(* compiler.sml
 * A compiler for Scheme, written in SML/NJ
 * as part of the compiler construction course
 *
 * Programmer: Mayer Goldberg, 2011
 *)
Control.Print.printDepth := 500000;
Control.Print.printLength := 500000;
Control.Print.stringDepth := 500000;
Control.polyEqWarn := false;

fun rel() = use("/home/lxmonk/Documents/school/BA2/compilers/hw/hw2/compiler.sml");

fun andmap f nil = true
  | andmap f (a :: s) = (f a) andalso (andmap f s);

fun ormap f nil = false
  | ormap f (a :: s) = (f a) orelse (ormap f s);

exception ErrorMap2UnequalLengths;

fun map2 f [] [] = []
  | map2 f (a :: s) (a' :: s') =
    (f(a, a')) :: (map2 f s s')
  | map2 f s s' = raise ErrorMap2UnequalLengths;

fun makeIsChar(string) =
    let val chars = explode(string)
    in
        fn ch => ormap (fn ch' => ch = ch') chars
    end;

fun makeCharInRange(charFrom, charTo) =
 fn ch : char => (charFrom <= ch) andalso (ch <= charTo);

fun stringEqual(string1, string2) =
    String.compare(string1, string2) = EQUAL;

fun stringNotEqual(str, str') =
    not(stringEqual(str, str'));

fun stringIsMember (str, list) =
    ormap (fn str' => stringEqual(str, str')) list;

fun stringNotAMember (str, list) =
    not (stringIsMember (str, list));

fun addStringToSet (str, []) = [str]
  | addStringToSet (str, s as (str' :: rest)) =
    if (stringEqual(str, str')) then s
    else str' :: (addStringToSet (str, rest));

fun unionStringSets ([], s') = s'
  | unionStringSets ((str :: s), s') =
    unionStringSets(s, (addStringToSet(str, s')));


datatype SchemeToken = LparenToken
                     | RparenToken
                     | QuoteToken
                     | DotToken
                     | VectorToken
                     | IntToken of int
                     | CharToken of char
                     | StringToken of string
                     | SymbolToken of string
                     | BoolToken of bool;

datatype Sexpr = Void
               | Nil
               | Pair of Sexpr * Sexpr
               | Vector of Sexpr list
               | Symbol of string
               | String of string
               | Number of int
               | Bool of bool
               | Char of char;

datatype Expr = Const of Sexpr
              | Var of string
              | VarFree of string               (* free variable *)
              | VarParam of string * int   (* parameter variable *)
              | VarBound of string * int * int (* bound variable *)
              | If of Expr * Expr * Expr
              | Abs of (string list) * Expr
              | AbsOpt of (string list) * string * Expr
              | AbsVar of string * Expr
              | App of Expr * (Expr list)
              | AppTP of Expr * (Expr list)  (* in tail position *)
              | Seq of Expr list
              | Or of Expr list
              | Set of Expr * Expr
              | Def of Expr * Expr;

signature SCANNER =
sig
    val stringToTokens : string -> SchemeToken list;
end;

signature READER =
sig
    val stringToSexpr : string -> Sexpr;
    val stringToSexprs : string -> Sexpr list;
end;

fun sexprToString'(Void) = "#<void>"
  | sexprToString'(Nil) = "()"
  | sexprToString'(Number(n)) = Int.toString(n)
  | sexprToString'(Char(#" ")) = "#\\space"
  | sexprToString'(Char(#"\t")) = "#\\tab"
  | sexprToString'(Char(#"\f")) = "#\\page"
  | sexprToString'(Char(#"\n")) = "#\\newline"
  | sexprToString'(Char(#"\r")) = "#\\return"
  | sexprToString'(Char(ch)) =
    if (ch > #" ") then "#\\" ^ Char.toString(ch)
    else let val n = ord(ch)
             val o3 = n mod 8
             val tmp = n div 8
             val o2 = tmp mod 8
             val o1 = tmp div 8
         in
             "#\\" ^
             Int.toString(o1) ^
             Int.toString(o2) ^
             Int.toString(o3)
         end
  | sexprToString'(Bool(true)) = "#t"
  | sexprToString'(Bool(false)) = "#f"
  | sexprToString'(String(str)) = "\"" ^ str ^ "\""
  | sexprToString'(Symbol(name)) = name
  | sexprToString'(Pair(Symbol("quote"),
                   Pair(e, Nil))) = "'" ^ sexprToString'(e)
  | sexprToString'(Pair(car, cdr)) = toStringWithCar(sexprToString'(car), cdr)
  | sexprToString'(Vector(s)) =
    "#(" ^ (String.concatWith " " (map sexprToString' s)) ^ ")"
and toStringWithCar(car, Nil) = "(" ^ car ^ ")"
  | toStringWithCar(car, Pair(first, second)) =
    toStringWithCar(car ^ " " ^ sexprToString'(first), second)
  | toStringWithCar(car, e) = "(" ^ car ^ " . " ^ sexprToString'(e) ^ ")"
and sexprToString(Void) = ""
  | sexprToString(e) = sexprToString'(e);

local
    val bintag = (fn tag => (fn str => "<" ^ tag ^ ">" ^ str ^ "</" ^ tag ^ ">"))
    val ital = bintag "I"
    val bold = bintag "B"
    val sub = bintag "SUB"
    val html = bintag "HTML"
    val head = bintag "HEAD"
    val body = bintag "BODY"
    val bintagAttr =
        (fn tag => (fn attr => (fn str => "<" ^ tag ^ " " ^ attr ^ ">" ^ str ^ "</" ^ tag ^ ">")))
    val red = bintagAttr "FONT" "COLOR='RED'"
    val blue = bintagAttr "FONT" "COLOR='BLUE'"
    val rec run =
        (fn (Const(sexpr)) => (sexprToString sexpr)
          | (Var(v)) => (ital v)
          | (VarFree(v)) => (ital v) ^
                            (sub "f")
          | (VarParam(v, mi)) => (ital v) ^
                                 (sub ("p" ^ (sub (red (Int.toString mi)))))
          | (VarBound(v, ma, mi)) => (ital v) ^
                                     (sub ("b" ^ (sub ((red (Int.toString ma)) ^
                                                       "," ^
                                                       (red (Int.toString mi))))))
          | (If(test, dit, (Const(Void)))) =>
            let val test = run test
                val dit = run dit
            in
                "(" ^ (bold "if") ^ " " ^ test ^ " " ^ dit ^ ")"
            end
          | (If(test, dit, dif)) =>
            let val test = run test
                val dit = run dit
                val dif = run dif
            in
                "(" ^ (bold "if") ^ " " ^ test ^ " " ^ dit ^ " " ^ dif ^ ")"
            end
          | (Abs(vars, expr)) =>
            let val vars = String.concatWith " " (map ital vars)
                val expr = run expr
            in
                "(&lambda; (" ^ vars ^ ") " ^ expr ^ ")"
            end
          | (AbsOpt(vars, var, expr)) =>
            let val vars = String.concatWith " " (map ital vars)
                val var = ital var
                val expr = run expr
            in
                "(&lambda; (" ^ vars ^ " . " ^ var ^ ") " ^ expr ^ ")"
            end
          | (AbsVar(var, expr)) =>
            let val var = ital var
                val expr = run expr
            in
                "(&lambda; " ^ var ^ " " ^ expr ^ ")"
            end
          | (App(proc, [])) =>
            let val proc = run proc
            in
                "(" ^ proc ^ ")"
            end
          | (App(proc, args)) =>
            let val proc = run proc
                val args = String.concatWith " " (map run args)
            in
                "(" ^ proc ^ " " ^ args ^ ")"
            end
          | (AppTP(proc, [])) =>
            let val proc = run proc
            in
                (blue "(") ^ proc ^ (blue ")")
            end
          | (AppTP(proc, args)) =>
            let val proc = run proc
                val args = String.concatWith " " (map run args)
            in
                (blue "(") ^ proc ^ " " ^ args ^ (blue ")")
            end
          | (Seq(exprs)) =>
            let val exprs = String.concatWith " " (map run exprs)
            in
                "(" ^ exprs ^ ")"
            end
          | (Or([])) => "(or)"
          | (Or(exprs)) =>
            let val exprs = String.concatWith " " (map run exprs)
            in
                "(" ^ (bold "or") ^ " " ^ exprs ^ ")"
            end
          | (Set(var, expr)) =>
            let val var = run var
                val expr = run expr
            in
                "(" ^ (bold "set!") ^ " " ^ var ^ " " ^ expr ^ ")"
            end
          | (Def(var, expr)) =>
            let val var = run var
                val expr = run expr
            in
                "(" ^ (bold "define") ^ " " ^ var ^ " " ^ expr ^ ")"
            end
        )
in
fun exprToHTMLString e =
    html((head "") ^ (body (run e)))
end;

signature TAG_PARSER =
sig
    val stringToPE : string -> Expr;
    val stringToPEs : string -> Expr list;
end;

exception NotAList of Sexpr;

fun schemeListToML Nil = []
  | schemeListToML (Pair(car, cdr)) = car :: (schemeListToML cdr)
  | schemeListToML e = raise NotAList(e);

fun MLListToScheme [] = Nil
  | MLListToScheme (a :: s) = Pair(a, (MLListToScheme s));

exception ErrorNothingAfterHash;
exception ErrorBadChar of char * string;
exception ErrorStringDoesntEnd of string;
exception ErrorNoMetaChar of string;
exception ErrorNoSuchMetaChar of string;
exception ErrorNoChar;
exception ErrorUnknownNamedChar of string;
exception ErrorHash of string;

structure Scanner : SCANNER =
struct
val whiteChar = makeIsChar(" \t\r\n");
val delimiterChar = makeIsChar("'()\";, \t\r\n");
val octalChar = makeCharInRange(#"0", #"7");
val upperChar = makeCharInRange(#"A", #"Z");
val lowerChar = makeCharInRange(#"a", #"z");
val digitChar = makeCharInRange(#"0", #"9");
val specialSymbolChar = makeIsChar("!@$%^*-_=+<>/?.&");

fun symbolChar (ch) =
    lowerChar(ch) orelse
    upperChar(ch) orelse
    digitChar(ch) orelse
    specialSymbolChar(ch);

local
    fun stInit([]) = []
      | stInit(#";" :: s) = stComment(s)
      | stInit(#"(" :: s) = LparenToken :: stInit(s)
      | stInit(#")" :: s) = RparenToken :: stInit(s)
      | stInit(#"'" :: s) = QuoteToken :: stInit(s)
      | stInit(#"#" :: s) = stHash(s)
      | stInit(#"." :: s) = DotToken :: stInit(s)
      | stInit(#"\"" :: s) = stString(s, [])
      | stInit(ch :: s) =
        if symbolChar(ch) then stSymbol(s, [ch])
        else if whiteChar(ch) then stInit(s)
        else raise ErrorBadChar(ch, implode(s))
    and stSymbol([], chars) =
        symbolOrNumberToken(charsToString(chars)) :: stInit([])
      | stSymbol(s as char :: s', chars) =
        if symbolChar(char) then stSymbol(s', char :: chars)
        else symbolOrNumberToken(charsToString(chars)) :: stInit(s)
    and stString([], chars) = raise ErrorStringDoesntEnd(charsToString(chars))
      | stString(#"\"" :: s, chars) =
        StringToken(charsToString(chars)) :: stInit(s)
      | stString(#"\\" :: s, chars) = stStringMetaChar(s, chars)
      | stString(ch :: s, chars) = stString(s, ch :: chars)
    and stStringMetaChar([], chars) =
        raise ErrorNoMetaChar(charsToString(chars) ^ "\\")
      | stStringMetaChar(#"t" :: s, chars) = stString(s, #"\t" :: chars)
      | stStringMetaChar(#"T" :: s, chars) = stString(s, #"\t" :: chars)
      | stStringMetaChar(#"r" :: s, chars) = stString(s, #"\r" :: chars)
      | stStringMetaChar(#"R" :: s, chars) = stString(s, #"\r" :: chars)
      | stStringMetaChar(#"n" :: s, chars) = stString(s, #"\n" :: chars)
      | stStringMetaChar(#"N" :: s, chars) = stString(s, #"\n" :: chars)
      | stStringMetaChar(#"f" :: s, chars) = stString(s, #"\f" :: chars)
      | stStringMetaChar(#"F" :: s, chars) = stString(s, #"\f" :: chars)
      | stStringMetaChar(#"Y" :: s, chars) =
        stString(s, [#"l",#"e",#"a",#"Y"] @ chars)
      | stStringMetaChar(#"\\" :: s, chars) = stString(s, #"\\" :: chars)
      | stStringMetaChar(#"\"" :: s, chars) = stString(s, #"\"" :: chars)
      | stStringMetaChar(ch :: s, chars) =
        raise ErrorNoSuchMetaChar("\\" ^ Char.toString(ch))
    and stHash([]) = raise ErrorNothingAfterHash
      | stHash(#"t" :: s) = BoolToken(true) :: stInit(s)
      | stHash(#"T" :: s) = BoolToken(true) :: stInit(s)
      | stHash(#"f" :: s) = BoolToken(false) :: stInit(s)
      | stHash(#"F" :: s) = BoolToken(false) :: stInit(s)
      | stHash(#"\\" :: s) = stChar(s)
      | stHash(#"(" :: s) = VectorToken :: stInit(s)
      | stHash(s) = raise ErrorHash("#\\" ^ implode(s))
    and stChar([]) = raise ErrorNoChar
      | stChar(ch :: s) = stChar'(s, [ch])
    and stChar'([], chars) = makeCharToken(chars) :: stInit([])
      | stChar'(s as ch :: s', chars) =
        if delimiterChar(ch) then makeCharToken(chars) :: stInit(s)
        else stChar'(s', ch :: chars)
    and stComment([]) = stInit([])
      | stComment(#"\n" :: s) = stInit(s)
      | stComment(ch :: s) = stComment(s)
    and charsToString(s) = implode(rev(s))
    and makeCharToken([ch]) = CharToken(ch)
      | makeCharToken(chars as [ch1, ch2, ch3]) =
        if (andmap octalChar chars) then
            CharToken(chr(digitToInt(ch1) +
                          8 * (digitToInt(ch2) +
                               8 * digitToInt(ch3))))
        else charNameToCharToken(charsToString(chars))
      | makeCharToken(chars) = charNameToCharToken(charsToString(chars))
    and charNameToCharToken(charName) =
        if stringEqual(charName, "space") then CharToken(#" ")
        else if stringEqual(charName, "return") then CharToken(#"\r")
        else if stringEqual(charName, "newline") then CharToken(#"\n")
        else if stringEqual(charName, "tab") then CharToken(#"\t")
        else if stringEqual(charName, "page") then CharToken(#"\f")
        else raise ErrorUnknownNamedChar(charName)
    and digitToInt(ch) = ord(ch) - ord(#"0")
    and symbolOrNumberToken(string) =
        case Int.fromString(string) of
            SOME (n) => IntToken(n)
          | NONE => SymbolToken(string)
in
fun stringToTokens(string) = stInit(explode(string))
end;
end; (* of structure Scanner *)

(* print ("***************************************************\n" ^ "TESTS:\n"); *)
(* Scanner.stringToTokens "()" = [LparenToken,RparenToken] ; *)
(* Scanner.stringToTokens "(a b c)" = *)
(*   [LparenToken,SymbolToken "a",SymbolToken "b",SymbolToken "c",RparenToken]; *)

(* Scanner.stringToTokens "(a . b)" = [LparenToken,SymbolToken "a",DotToken,SymbolToken "b",RparenToken]; *)

(*           Scanner.stringToTokens "(define abs (lambda (x) (if (negative? x) (- x) x)))" = [LparenToken,SymbolToken "define",SymbolToken "abs",LparenToken, *)
(*                                                                                SymbolToken "lambda",LparenToken,SymbolToken "x",RparenToken,LparenToken, *)
(*                                                                                SymbolToken "if",LparenToken,SymbolToken "negative?",SymbolToken "x", *)
(*                                                                                RparenToken,LparenToken,SymbolToken "-",SymbolToken "x",RparenToken, *)
(*                                                                                SymbolToken "x",RparenToken,RparenToken,RparenToken] ; *)
(* Scanner.stringToTokens "#\\A" = [CharToken #"A"] ; *)
(* Scanner.stringToTokens "#\\space" = [CharToken #" "] ; *)
(* Scanner.stringToTokens "#\\return" = [CharToken #"\r"]; *)
(* Scanner.stringToTokens "#\\newline" = [CharToken #"\n"]; *)
(* Scanner.stringToTokens "#\\tab" = [CharToken #"\t"] ; *)
(* Scanner.stringToTokens ")(." = [RparenToken,LparenToken,DotToken] ; *)
(* Scanner.stringToTokens "#(a b c)" = *)
(*   [VectorToken,SymbolToken "a",SymbolToken "b",SymbolToken "c",RparenToken]; *)

(* Scanner.stringToTokens "#(a b . c)" = *)
(*   [VectorToken,SymbolToken "a",SymbolToken "b",DotToken,SymbolToken "c", *)
(*    RparenToken] ; *)

(* print "TESTS:\n***************************************************\n"; *)



(* fun slurpSexpr(tokens, sexprSoFar, ~1) = raise BadSExpression(rev sexprSoFar) *)
(*   | slurpSexpr ([], sexprSoFar, openParens) = *)
(*     raise BadSExpression(rev sexprSoFar) *)
(*   | slurpSexpr (LparenToken :: tokens, sexprSoFar, openParens) = *)
(*     slurpSexpr (tokens, LparenToken::sexprSoFar, openParens + 1) *)
(*   | slurpSexpr (VectorToken :: tokens, sexprSoFar, openParens) = *)
(*     slurpSexpr (tokens, VectorToken::sexprSoFar, openParens + 1) *)
(*   | slurpSexpr (RparenToken :: tokens, sexprSoFar, 1) = *)
(*     (rev (RparenToken::sexprSoFar), tokens) *)
(*   | slurpSexpr (RparenToken :: tokens, sexprSoFar, openParens) = *)
(*     (* not balanced yet *) *)
(*     slurpSexpr (tokens, RparenToken::sexprSoFar, openParens - 1) *)
(*   | slurpSexpr (token :: tokens, sexprSoFar, 0) = *)
(*     ([token], tokens) *)
(*   | slurpSexpr (token :: tokens, sexprSoFar, openParens) = *)
(*     slurpSexpr (tokens, token::sexprSoFar, openParens); (* inside Sexpr *) *)

fun listToPairs ([], b) = b
  | listToPairs (a::s, b) = (* (print "listToPairs!!"; *)
    Pair(a, (listToPairs(s, b)));
    
exception BadSExpression of SchemeToken list;
exception BadSExpressionQuote of SchemeToken list;
exception BadSExpressionVector of SchemeToken list;
exception BadSExpressionLparen of SchemeToken list;
exception BadSExpressionGss of SchemeToken list;
exception TokenListToSexprError of SchemeToken list;

structure Reader : READER =
struct
(* val stringToSexpr : string -> Sexpr; *)
(* val stringToSexprs : string -> Sexpr list; *)
local
    fun gs [] = (NONE, [])
      | gs (toks as RparenToken::_) = (NONE, toks)
      | gs (toks as DotToken::_) = (NONE, toks)
      | gs (IntToken(n)::toks) = (SOME (Number(n)), toks)
      | gs (CharToken(c)::toks) = (SOME (Char(c)), toks)
      | gs (StringToken(str)::toks) = (SOME (String(str)), toks)
      | gs (SymbolToken(sym)::toks) = (SOME (Symbol(sym)), toks)
      | gs (BoolToken(bool)::toks) = (SOME (Bool(bool)), toks)
      | gs (QuoteToken :: toks) =
        (case (gs toks) of
             (SOME(sexpr), toks) =>
             (SOME(Pair(Symbol("quote"),
                        Pair(sexpr, Nil))),
              toks)
           | _ => raise BadSExpressionQuote(toks))
      | gs (VectorToken::toks) =
        (case (gss (gs toks)) of
             (sexprs, (RparenToken::toks)) =>
             (SOME(Vector(sexprs)), toks)
           | _ => raise BadSExpressionVector(toks))
      | gs (LparenToken::toks) =
        (case (gss (gs toks)) of
             (sexprs, (RparenToken::toks)) =>
             (SOME(listToPairs(sexprs, Nil)), toks)
           | (sexprs as (_::_), DotToken::toks) =>
             (case (gs toks) of
                  (SOME(sexpr), RparenToken::toks) =>
                  (SOME(listToPairs(sexprs,sexpr)),toks)
                | _ => raise BadSExpressionLparen(toks))
           | _ => raise BadSExpressionLparen(toks)) (* this line +-+- *)
    and gss (NONE, toks) = ([] : Sexpr list, toks)
      | gss (SOME(sexpr), []) = ([sexpr],[])
      | gss (SOME(sexpr), toks) =
        let val ret = gss (gs toks)
        in
            (sexpr::(#1 ret), (#2 ret))
        end
in

val stringToSexpr = fn str =>   (* (sexpr, []) = sexpr; or raise error *)
                       let val schemeTokensList = Scanner.stringToTokens(str)
                       in
                           (case (gs schemeTokensList) of
                                (SOME(ret) : Sexpr option, []) => ret
                              | _ => raise BadSExpression(schemeTokensList))
                       end
val stringToSexprs = fn str =>
                        let val schemeTokensList = Scanner.stringToTokens(str)
                        in
                            (case (gss (gs schemeTokensList)) of
                                 (ret : Sexpr list, []) => ret
                               | _ => raise BadSExpression(schemeTokensList))
                        end
end; (* of local functions *)
end; (* of structure Reader *)


Reader.stringToSexpr "(a)" = Pair (Symbol "a",Nil);
Reader.stringToSexpr "(a b . c)" = Pair (Symbol "a",Pair (Symbol "b",Symbol "c"));
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
                             Pair (Symbol "x",Nil)))),Nil))),Nil)));
Reader.stringToSexpr "#(a b c)" = Vector [Symbol "a",Symbol "b",Symbol "c"];
Reader.stringToSexpr "()" = Nil;

structure TagParser : TAG_PARSER =
struct
val reservedSymbols = ["and", "begin", "cond", "define", "else",
                       "if", "lambda", "let", "let*", "letrec",
                       "or", "quote", "set!"];

fun reservedWord(str) =
    ormap (fn rs => (String.compare(rs, str) = EQUAL)) reservedSymbols;

val stringToPE = fn str => Var("not implemented")
val stringToPEs = fn str => [Var("not implemented"), Var("yet!")]
end; (* of structure TagParser *)

(* local *)
(*     fun tokenListToSexpr [LparenToken, token1, DotToken, token2, RparenToken] = *)
(*         (print "1\n"; Pair(tokenListToSexpr [token1], *)
(*                            tokenListToSexpr [token2]))(*dotted pair*) *)
(*       (* | tokenListToSexpr [token1, DotToken, token2, RparenToken] = *) *)
(*       (*   (print "2\n"; Pair(tokenListToSexpr [token1], tokenListToSexpr [token2])) *) *)
(*       | tokenListToSexpr [DotToken, token1, RparenToken] = *)
(*         (print "3\n"; tokenListToSexpr [token1]) *)
(*       | tokenListToSexpr [SymbolToken(symbol)] = (print ("symbol: "^symbol^"\n"); Symbol(symbol)) *)
(*       | tokenListToSexpr [StringToken(string)] = (print "string"; String(string)) *)
(*       | tokenListToSexpr [BoolToken(bool)]     = (print "bool"; Bool(bool)) *)
(*       | tokenListToSexpr [CharToken(char)]     = (print "char"; Char(char)) *)
(*       | tokenListToSexpr [IntToken(num)]       = (print "num"; Number(num)) *)
(*       | tokenListToSexpr [LparenToken, RparenToken] = Nil (* [] *) *)
(*       | tokenListToSexpr [LparenToken, token, RparenToken] = *)
(*         (print "4\n"; Pair(tokenListToSexpr [token], Nil) (* singleton *)) *)
(*       | tokenListToSexpr ((* LparenToken :: *) LparenToken :: tokens) = (* (( *) *)
(*         let val sexprAndRest = slurpSexpr(LparenToken::tokens, [], 0) *)
(*         in *)
(*             (print "5\n"; MLListToScheme(tokenListToSexprs (tl (#1 sexprAndRest)) @ *)
(*                            tokenListToSexprs((#2 sexprAndRest)))) *)
(*         end *)
(*       | tokenListToSexpr ((* LparenToken :: *) VectorToken :: tokens) = (* (#( *) *)
(*         let val sexprAndRest = slurpSexpr(VectorToken :: tokens, [], 0) *)
(*         in *)
(*             (print "vector\n"; Pair(tokenListToSexpr (tl (#1 sexprAndRest)), *)
(*                  tokenListToSexpr (#2 sexprAndRest))) *)
(*         end *)
(*       | tokenListToSexpr (RparenToken :: []) = (print "RparenToken"; Nil) *)
(*       | tokenListToSexpr err = raise TokenListToSexprError(err)(* (print("oops\n"); Void) *) *)
(*     and tokenListToSexprs [] = (print "s-[]\n"; [Nil]) *)
(*       | tokenListToSexprs [RparenToken] = [] (* end the list *) *)
(*       | tokenListToSexprs (VectorToken :: tokens) = *)
(*         let val sexprAndRest = slurpSexpr(VectorToken::tokens, [], 0) *)
(*         in *)
(*             tokenListToSexpr (#1 sexprAndRest) :: *)
(*             tokenListToSexprs (#2 sexprAndRest) *)
(*         end *)
(*       | tokenListToSexprs (LparenToken :: tokens) = *)
(*         let val sexprAndRest = slurpSexpr(LparenToken::tokens, [], 0) *)
(*         in *)
(*             tokenListToSexpr (tl (#1 sexprAndRest)) :: *)
(*             tokenListToSexprs (#2 sexprAndRest) *)
(*         end *)
(*       | tokenListToSexprs (DotToken :: token :: RparenToken :: tokens) = *)
(*         tokenListToSexpr([token]) :: tokenListToSexprs(tokens) *)
(*       | tokenListToSexprs (token :: tokens) = *)
(*         let val sexprAndRest = slurpSexpr(token::tokens, [], 0) *)
(*         in *)
(*             tokenListToSexpr((#1 sexprAndRest)) :: *)
(*             tokenListToSexprs((#2 sexprAndRest)) *)
(*         end *)
(* (* | tokenListToSexprs err =  raise TokenListToSexprError(err)(* [Void, Void] *) *) *)

(*       (* | tokenListToSexpr (LparenToken :: token1 :: token2 :: tokens) = (*list*) *) *)
(*       (*   Pair(tokenListToSexpr [token1], tokenListToSexpr (LparenToken :: *) *)
(*       (*                                   token2 :: tokens)) *) *)
(*       (* | tokenListToSexpr (VectorToken :: tokens) = *) *)
(* (*   Vector(tokenListToSexprs tokens) (* vector *) *) *)
(* in *)
)
