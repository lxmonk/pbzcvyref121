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

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;
val debug = true            
fun rel() = use("/Users/admin/gmayer/work/lang/ml/compiler.sml");
(* fun rel() = use("/home/lxmonk/Documents/school/BA2/compilers/hw/hw2/compiler.sml"); *)

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
    val tokensToSexprs : SchemeToken list -> Sexpr;
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
and sexprToString(Void) = "#<void>" (* FIXME: WAS "" *)
  | sexprToString(e) = sexprToString'(e);

(* FIXME TODO DELETE THESE *)
val rec stringlistToString = fn(nil) => ""
		             | (h::nil) => h
		             | (h::t) => h ^ " " ^ stringlistToString(t);
val rec exprToString = fn(Const x) => sexprToString(x)
	               | Var x => x
	               | VarFree x => x
	               | VarParam (x, n) => x
	               | VarBound (x,n1,n2) => x
	               | If (X ,Y ,Z) => "(if " ^ exprToString(X) ^" "^ exprToString(Y) ^" "^ exprToString(Z) ^ ")" 
		       | Abs(Slist,E)=> "(lambda ("^stringlistToString(Slist)^") " ^ exprToString(E) ^ ")"
		       | AbsOpt(SList,S,E) => "(lambda ("^stringlistToString(SList)^" . " ^ S ^ ") " ^ exprToString(E) ^ ")"
		       | AbsVar(S,E) => "(lambda " ^ S ^ exprToString(E) ^ ")"
		       | App(X,XList) => "("^exprToString(X) ^" "^ stringlistToString(map exprToString XList) ^ ")"
		       | AppTP(X,XList) => "("^exprToString(X) ^" "^ stringlistToString(map exprToString XList) ^ ")"
		       | Seq(XList) => "(begin " ^ stringlistToString(map exprToString XList) ^ ")"
		       | Or(XList) => "(or " ^ stringlistToString(map exprToString XList) ^ ")"
		       | Set(X,Y) => "(set! " ^exprToString(X) ^" "^ exprToString(Y)^" )"
		       | Def(X,Y) => "(define " ^exprToString(X) ^" "^ exprToString(Y)^")";

(* FIXME TODO DELETE THESE *)		  
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
    val parse : Sexpr -> Expr;  (* TODO: FIXME!! *)
    val createNestedIfs : Sexpr list -> Expr;  (* TODO: FIXME!! *)
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

fun listToPairs ([], b) = b
  | listToPairs (a::s, b) = 
    Pair(a, (listToPairs(s, b)));

    
exception PairsToListError;
          
fun pairsToList (Nil) = []
  | pairsToList (Pair(p1, p2)) = p1 :: pairsToList(p2)
  | pairsToList err = raise PairsToListError;
    
exception BadSExpression of SchemeToken list;
exception BadSExpressionQuote of SchemeToken list;
exception BadSExpressionVector of SchemeToken list;
exception BadSExpressionLparen of SchemeToken list;
exception BadSExpressionGss of SchemeToken list;
exception TokenListToSexprError of SchemeToken list;

structure Reader : READER =
struct
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
val  tokensToSexprs = fn schemeTokensList =>
                        (case (gs schemeTokensList) of
                             (SOME(ret) : Sexpr option, []) => ret
                           | _ => raise BadSExpression(schemeTokensList))
                        
end; (* of local functions *)
end; (* of structure Reader *)



    
structure TagParser : TAG_PARSER =
struct
exception MissingFeatureException;
exception ErrorTypingLamb;
exception ErrorReservedWordUsedImproperly;
exception BadAndExpression;
exception NewLambdaMark;
exception LetGetVarsError;
exception BreakLetsError;
exception BadCondExpression;
exception LetStarError;
exception CreateNestedLetsError;
exception InterlaceError;
          
val reservedSymbols = ["and", "begin", "cond", "define", "else",
                       "if", "lambda", "let", "let*", "letrec",
                       "or", "quote", "set!"];

fun reservedWord(str) =
    ormap (fn rs => (String.compare(rs, str) = EQUAL))
          reservedSymbols;

datatype lamb = Sim of string list
              | Opt of (string list * string)
              | Vard of string;

fun lambtype (Nil : Sexpr) = Sim []
  | lambtype (Pair(Symbol(str), rest)) =
    (case (lambtype rest) of
         Sim(vars) => Sim(str::vars)
       | Opt(vars, var) => Opt((str::vars), var)
       | Vard(var) => Opt([str],var))
  | lambtype (Symbol(str)) = Vard(str)
  | lambtype (err : Sexpr) = raise ErrorTypingLamb;

fun interlace (Pair(lastVar, Nil), const) =
    Pair(Pair(lastVar, Pair(const, Nil)), Nil)
  | interlace (Pair(var, vars), const) =
    Pair(Pair(var, Pair(const, Nil)), interlace(vars, const))
  | interlace _ = raise InterlaceError;

fun slurpSexpr(tokens, sexprSoFar, ~1) =
    raise BadSExpression(rev sexprSoFar)
  | slurpSexpr ([], sexprSoFar, openParens) =
    raise BadSExpression(rev sexprSoFar)
  | slurpSexpr (LparenToken :: tokens, sexprSoFar, openParens) =
    slurpSexpr (tokens, LparenToken::sexprSoFar, openParens + 1)
  | slurpSexpr (VectorToken :: tokens, sexprSoFar, openParens) =
    slurpSexpr (tokens, VectorToken::sexprSoFar, openParens + 1)
  | slurpSexpr (RparenToken :: tokens, sexprSoFar, 1) =
    (rev (RparenToken::sexprSoFar), tokens)
  | slurpSexpr (RparenToken :: tokens, sexprSoFar, openParens) =
    (* not balanced yet *)
    slurpSexpr (tokens, RparenToken::sexprSoFar, openParens - 1)
  | slurpSexpr (token :: tokens, sexprSoFar, 0) =
    ([token], tokens)
  | slurpSexpr (token :: tokens, sexprSoFar, openParens) =
    slurpSexpr (tokens, token::sexprSoFar, openParens); (* inside Sexpr *)

fun listOfSexprs ([]) = []
  | listOfSexprs (sexprs) =
    let val sexprAndRest = slurpSexpr(sexprs, [], 0)
    in
        (#1 sexprAndRest) :: listOfSexprs(#2 sexprAndRest)
    end    

fun parse Void = Const(Void)
  | parse Nil = Const(Nil)
  | parse (Vector(sexprs)) = Const(Vector(sexprs)) (* FIXME! *)
  | parse (str as String(s)) = Const(str)
  | parse (n as Number(num)) = Const(n)
  | parse (b as Bool(bool)) = Const(b)
  | parse (c as Char(ch)) = Const(c)
  | parse (Symbol(sym)) = (case (reservedWord(sym)) of
                               false => Var(sym)
                             | true =>
                               raise ErrorReservedWordUsedImproperly)
  | parse (Pair(Symbol("quote"), Pair(s as Symbol(sym),Nil))) =
    Const(s)
  | parse (Pair(Symbol("quote"), Pair(n as Number(num),Nil))) =
    Const(n)
  | parse (Pair(Symbol("quote"), Pair(v as Vector(elements), Nil))) =
    Const(v)
  | parse (Pair(Symbol("quote"), Pair(c as Char(ch), Nil))) = Const(c)
  | parse (Pair(Symbol("quote"), Pair(b as Bool(bo), Nil))) = Const(b)
  | parse (Pair(Symbol("quote"), Pair(Nil, Nil))) = Const(Nil)
  | parse (Pair(Symbol("quote"), Pair(sexpr,Nil))) = Const(sexpr)
  | parse (Pair(Symbol("lambda"), d as Pair(vars,Pair(body,Nil)))) =
    (case lambtype(vars) of
         Sim(varlist) =>
         Abs(varlist,parse(Pair(Symbol("begin"),Pair(body, Nil))))
       | Opt(vars, var) =>
         AbsOpt(vars, var, parse(Pair(Symbol("begin"), Pair(body,
                                                            Nil))))
       | Vard(var) =>
         AbsVar(var, parse(Pair(Symbol("begin"), Pair(body, Nil)))))
  | parse (Pair(Symbol("lambda"), d as Pair(vars,body))) =
    (case lambtype(vars) of
         Sim(varlist) => 
         Abs(varlist,parse(Pair(Symbol("begin"),body)))
       | Opt(vars, var) =>
         AbsOpt(vars, var, parse(Pair(Symbol("begin"), body)))
       | Vard(var) =>
         AbsVar(var, parse(Pair(Symbol("begin"), body))))
  | parse (Pair(Symbol("if"), Pair(test, Pair(dit,Nil)))) =
    parse (Pair(Symbol("if"), Pair(test, Pair(dit, Pair(Void, Nil)))))
  | parse (Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil))))) =
    If(parse(test), parse(dit), parse(dif))
  | parse (Pair(Symbol("define"), Pair(sym as Symbol(defined_symbol),
                                       definition))) =
     Def(parse(sym), parse(Pair(Symbol("begin"), definition)))
  | parse (Pair(def as Symbol("define"),    (* MIT define *)
                Pair(Pair(sym as Symbol(defined_symbol), vars),
                     Pair(body, Nil)))) =
    parse (Pair(def, Pair(sym, Pair(Pair(Symbol("lambda"),
                                         Pair(vars,
                                              Pair(body, Nil))),
                                    Nil))))
  | parse (Pair(Symbol("begin"), body)) =
    (case (body) of
         Nil => Const(Void)
       | (Pair (b, Nil)) => parse(b)
       | (bod as Pair(p1, p2)) => Seq(map parse (pairsToList bod))
       | _ => parse (body))
  | parse (Pair(Symbol("or"), elements)) =
    (case elements of
         Nil => Or([])
       | _ => Or(map parse (pairsToList elements)))
  | parse (Pair(Symbol("and"), elements)) =
    (case elements of
         Nil => Const(Bool(true))
       | _ => createNestedIfs (pairsToList elements))
  | parse (Pair(Symbol("set!"), Pair(var, Pair(value, Nil)))) =
    Set(parse(var), parse(value))
  | parse (Pair(Symbol("cond"), bodies)) =
    createCond(bodies)
  | parse (Pair(Symbol("let"),Pair(Nil, applic))) =
    parse(Pair(Symbol("begin"), applic))
  | parse (Pair(Symbol("let"),lets)) =
    let val brokenLet = breakLets(lets)
    in
        parse(
        Pair(Pair(Symbol("lambda"),
                  Pair((#1 brokenLet),
                       Pair(Pair(Symbol("begin"),
                                 (#3 brokenLet)),
                            Nil))), (#2 brokenLet)))
    end
  | parse (Pair(Symbol("let*"), lets)) =
    (case lets of
         Pair(Nil, applic) => parse(Pair(Symbol("begin"), applic))
       | Pair(Pair(onlyRib, Nil), applic) => (* let* of depth 1 *)
         parse (Pair(Symbol("let"), lets))
       | Pair(Pair(rib, ribs), applic) =>
         parse(Pair(Symbol("let"),
                    Pair(Pair(rib, Nil),
                         createNestedLets(ribs, applic))))
       | _ => raise LetStarError)
  | parse (Pair(Symbol("letrec"), bod as Pair(Nil, applic))) =
    parse(Pair(Symbol("begin"), applic))
  | parse (Pair(Symbol("letrec"), bod as Pair(ribs, applic))) =
    let val vars = (#1 (getVars(ribs, [], [])))
        val newRibs = interlace(vars, Void)
        val sets = (listToPairs ((map (fn el => Pair(Symbol("set!"), el))
                                      (pairsToList ribs)), Nil))
    in
        parse(Pair(Symbol("let"),
                   Pair(newRibs,
                        Pair(Pair(Symbol("begin"),
                                  sets),
                             Pair(Pair(Pair(Symbol("lambda"),
                                            Pair(Nil, applic)),
                                       Nil), Nil)))))
    end
  (* these are always the last two lines *)
  | parse (Pair(sym as Symbol(operator), operands)) =
    App(parse(sym), map parse (pairsToList operands))
  | parse (Pair(operator, operands)) =
    App(parse(operator), map parse (pairsToList operands))
and createNestedLets (Pair(lastRib as Pair(variable,
                                           Pair(value, Nil)),
                           Nil), applic) =
    Pair(Pair(Symbol("let"), Pair(Pair(lastRib, Nil), applic)), Nil)
  | createNestedLets (Pair(rib, ribs), applic) =
    Pair(Pair(Symbol("let"), Pair(Pair(rib, Nil),
                                  createNestedLets(ribs, applic))), Nil)
  | createNestedLets _ = raise CreateNestedLetsError
and createNestedIfs [sexpr] = If(parse(sexpr), parse(sexpr),
                                 Const(Bool(false)))
  | createNestedIfs (sNextToLast::[sLast]) =
    If(parse(sNextToLast), parse(sLast), Const(Bool(false)))
  | createNestedIfs (sexpr::sexprs) =
    If(parse(sexpr), createNestedIfs(sexprs), Const(Bool(false)))
  | createNestedIfs _ = raise BadAndExpression
and createCond (Pair(Pair(Symbol("else"),body), _)) =
    parse(Pair(Symbol("begin"), body))       (* else clause *)  
  | createCond (Pair(Pair(test, body),Nil)) =
    (* last (but NOT else!!) clause *)
    If(parse(test), parse(Pair(Symbol("begin"),body)),
       Const(Void))
  | createCond (Pair(Pair(test, body),nextRibs)) =
    If(parse(test), parse(Pair(Symbol("begin"), body)),
       createCond(nextRibs))
  | createCond _ = raise BadCondExpression
and breakLets (Pair(ribs, applic)) =
    let val varsAndVals = getVars(ribs, [], [])
    in
        ((#1 varsAndVals), (#2 varsAndVals), applic)
    end
  | breakLets _ = raise BreakLetsError
and getVars (Pair(rib as Pair(variable, Pair(value, Nil)), Nil),
             varList, valList) =
    (listToPairs (rev (variable :: varList), Nil),
     listToPairs (rev (value :: valList), Nil))
  | getVars (Pair(rib as Pair(variable, Pair(value, Nil)), ribs),
             varList, valList) =
    getVars (ribs, variable :: varList, value :: valList)
  | getVars (err, _, _) = raise LetGetVarsError
        
val stringToPE = fn str => (* Var("not implemented") *)
                    let val sexpr = Reader.stringToSexpr(str)
                    in
                        parse(sexpr)
                    end
val stringToPEs = fn str =>
                     let val tokensList = Scanner.stringToTokens str
                         val schemeTokensListList =
                             listOfSexprs tokensList
                         val sexprsList =
                             map Reader.tokensToSexprs schemeTokensListList
                     in
                         map parse sexprsList
                     end

end; (* of structure TagParser *)


signature SEMANTIC_ANALYSIS =
sig
    (* val bangFinder : string list * Expr * string list -> string list *)
    val boxSet : Expr -> Expr;
    val annotateTC : Expr -> Expr;
    val lexicalAddressing : Expr -> Expr;
    val analysis : Expr -> Expr;
end;


structure SemanticAnalysis : SEMANTIC_ANALYSIS =
struct
local
    exception NotYetImplemented;
    exception AbsBoxingException;
    exception AbsOptBoxingException;
    exception AbsVarBoxingException;
    exception LookupError;
    exception LexicalWorkerError;
    exception AnnotateTCRunErr;
    exception ErrorInPlacer;
    exception ErrorInLexicalAddressing;


    fun getIdx (el, [], n) = NONE
      | getIdx (el, h::tl, n) =
        if el = h then SOME(n)
        else getIdx(el, tl, n+1);

    fun lookup (name, [], row) = NONE
      | lookup (name, symtab, row) =
        if row >= (length symtab)   (* search exhausted *)
        then NONE
        else
            (case (getIdx(name, List.nth (symtab, row), 0)) of
                 NONE => lookup(name, symtab, row+1)
               | SOME(i) => SOME(row - 1, i));

    fun lexicalWorker (Var(name), symtab) =
        (case (lookup(name, symtab, 0)) of
             NONE => VarFree(name)
           | SOME((~1, index)) => VarParam(name, index)
           | SOME(i1, i2) => VarBound(name, i1, i2))
      | lexicalWorker (Abs(strList, expr), symtab) =
        Abs(strList, lexicalWorker(expr,
                                   strList :: symtab))
      | lexicalWorker (AbsOpt(strList, FIXME, expr), symtab) =
        AbsOpt(strList, FIXME, lexicalWorker(expr,
                                             strList :: symtab))
      | lexicalWorker (AbsVar (s, expr), symtab) =
        AbsVar (s, lexicalWorker(expr, [s] :: symtab))
      | lexicalWorker (If(e1, e2, e3), symtab) =
        If(lexicalWorker(e1, symtab),
           lexicalWorker(e2, symtab),
           lexicalWorker(e3, symtab))
      | lexicalWorker (App(operator, operands), symtab) =
        App(lexicalWorker(operator, symtab),
            (map (fn operand => (lexicalWorker (operand, symtab))) operands))
      | lexicalWorker (AppTP(operator, operands), symtab) =
        AppTP(lexicalWorker(operator, symtab),
              (map (fn operand => (lexicalWorker (operand, symtab))) operands))
      | lexicalWorker (Seq(exprList), symtab) =
        Seq((map (fn expr => (lexicalWorker (expr, symtab)))
                 exprList))
      | lexicalWorker (Or([]), symtab) =
        lexicalWorker(Const(Bool(false)), symtab)
      | lexicalWorker (Or(exprList), symtab) =
        Or((map (fn expr => (lexicalWorker (expr, symtab)))
                exprList))
      | lexicalWorker (Set(e1, e2), symtab) =
        Set(lexicalWorker(e1, symtab),
            lexicalWorker(e2, symtab))
      | lexicalWorker (Def(e1, e2), symtab) =
        Def(lexicalWorker(e1, symtab),
            lexicalWorker(e2, symtab))
      | lexicalWorker (Const(sexpr), _) = Const(sexpr)
      | lexicalWorker (e as VarFree(_), _) = e
      | lexicalWorker (e as VarBound(_), _) = e
      | lexicalWorker (e as VarParam(_), _) = e

    val prn = foldr (fn (a, b) => a ^ ", " ^ b) "";

    fun run (pe as Const(_), inTP) = pe
      | run (pe as Var(_), inTP) = pe
      | run (pe as VarFree(_), inTP) = pe
      | run (pe as VarParam(_), inTP) = pe
      | run (pe as VarBound(_), inTP) = pe
      | run (If(test, dit, dif), inTP) =
        If((run (test, false)),
           (run (dit, inTP)),
           (run (dif, inTP)))
      | run (Seq([last]), inTP) =
        Seq([(run(last, inTP))])
      | run (Seq(lst), inTP) =
        let val last = List.last(lst)
            val len = List.length(lst)
            val butlast = List.take(lst, len - 1)
        in
            Seq(((map (fn element => run(element, false)) butlast))  @ 
                [run(last, inTP)])
        end
      | run (Or([last]), inTP) =
        Or([(run(last, inTP))])
      | run (Or(lst), inTP) =
        let val last = List.last(lst)
            val len = List.length(lst)
            val butlast = List.take(lst, len - 1)
        in
            Or((map (fn element => (run(element, false)))
                    butlast) @ [(run(last, inTP))])
        end
      | run (Set(e1, e2), inTP) =
        Set(e1, run(e2, false))
      | run (Def(e1, e2), inTP) =
        Def(e1, run(e2, false))
      | run (App(operator, operands), false) =
        App((run (operator, false)),
            (map (fn arg => run(arg, false)) operands))
      | run (App(operator, operands), true) =
        AppTP((run (operator, false)),
              (map (fn arg => run(arg, false)) operands))
      | run (Abs(vars, body), inTP) =
        Abs(vars, run(body, true))
      | run (AbsOpt(vars, extra, body), inTP) =
        AbsOpt(vars, extra, run(body, true))
      | run (AbsVar(var, body), inTP) =
        AbsVar(var, run(body, true))
      | run err = raise AnnotateTCRunErr;


    fun inVarList (var, []) = NONE
      | inVarList (var, h::tl) =
        if var = h then SOME(var)
        else
            inVarList(var, tl);

    fun union ([] : string list list, result : string list) =
        result
      | union (([] :: lists : string list list), result) =
        union (lists, result)
      | union (((l : string) :: (lst : string list)) ::
               (lists : string list list),
               result) =
        union(lst :: lists, l :: result)
        
    fun unique ([], result) = result
      | unique (l::lst, result) =
        (case (inVarList(l, result)) of
             NONE => unique(lst, l::result)
           | SOME(l) => unique(lst, result));

    fun intersection ([], lst, result) = result
      | intersection (s::strs, lst, result) =
        (case inVarList(s, lst) of
             NONE => intersection(strs, lst, result)
           | SOME(s) => intersection(strs, lst, s::result))
    val car = VarFree("car");
    val set_car = VarFree("set-car!");
        
    fun boxVarInExpr (e as Const(_), _ : string) = e
      | boxVarInExpr (e as VarFree(_), _) = e
      | boxVarInExpr (e as VarBound(name, i1, i2), var) =
        if var = name
        then
            App(car, [e])
        else
            e
      | boxVarInExpr (e as VarParam(name, i), var) =
        if var = name
        then
            App(car, [e])
        else
            e
      | boxVarInExpr (e as Set(pVar as VarParam(name, i),
                               value), var) =
        if var = name
        then
            (App(set_car, pVar :: [boxVarInExpr(value, var)]))
        else
            Set(pVar, boxVarInExpr(value, var))
      | boxVarInExpr (e as Set(bVar as VarBound(name, i1, i2),
                               value), var) =
        if var = name
        then
            (App(set_car, bVar :: [boxVarInExpr(value, var)]))
        else
            Set(bVar, boxVarInExpr(value, var))
      | boxVarInExpr (e as Set(VarFree(name), value), var) = e
      | boxVarInExpr (If(test, dit, dif), var) =
        If(boxVarInExpr(test, var),
           boxVarInExpr(dit, var),
           boxVarInExpr(dif, var))
      | boxVarInExpr (Abs(vars, body), var) =
        (case (inVarList(var, vars)) of
             NONE => Abs(vars, boxVarInExpr(body, var))
           | SOME(var) => Abs(vars, body))
      (* this var is re-bound by the lambda, 
       so no more replacing *)
      | boxVarInExpr (AbsOpt(vars, optVar, body), var) =
        (case (inVarList(var, vars)) of
             NONE => Abs(vars, boxVarInExpr(body, var))
           | SOME(var) => AbsOpt(vars, optVar, body))
      (* this var is re-bound by the lambda, 
       so no more replacing *)
      | boxVarInExpr (e as AbsVar(varListName, body), var) =
        if var = varListName
        then
            e
        else
            AbsVar(varListName, boxVarInExpr(body, var))
      | boxVarInExpr (App(operator, operands), var) =
        App(boxVarInExpr(operator, var),
            (map (fn arg => boxVarInExpr(arg, var)) operands))
      | boxVarInExpr (AppTP(operator, operands), var) =
        AppTP(boxVarInExpr(operator, var),
              (map (fn arg => boxVarInExpr(arg, var)) operands))
      | boxVarInExpr (Seq(exprs), var) =
        Seq(map (fn expr => boxVarInExpr(expr, var)) exprs)
      | boxVarInExpr (Or(exprs), var) =
        Or(map (fn expr => boxVarInExpr(expr, var)) exprs)
      | boxVarInExpr (Def(deffed, value), var) =
        Def(boxVarInExpr(deffed, var),
            boxVarInExpr(value, var))
      | boxVarInExpr (Var(_), var) = raise ErrorInLexicalAddressing
      | boxVarInExpr (Set(_), var) = raise ErrorInLexicalAddressing;

    fun placer (var : string, [], i) = raise ErrorInPlacer
      | placer (var, v::vars, i) =
        if var = v
        then
            i
        else
            placer(var, vars, i+1);
        
    fun placeFinder(var, vars) = placer(var, vars, 0);
        
    fun boundVarsFinder ([], _, bound) = bound (*found all of them*)
      | boundVarsFinder (vars, Const(_), bound) = bound
      | boundVarsFinder (vars, Var(_), bound) =
        bound
      | boundVarsFinder (vars, VarFree(_), bound) = bound
      | boundVarsFinder (vars, VarBound(name, _, _), bound) =
        (case (inVarList(name, vars)) of
             NONE => bound
           | SOME(name) => name :: bound)
      | boundVarsFinder (vars, VarParam(_), bound) = bound
      | boundVarsFinder (vars, If(test, dit, dif), bound) =
        unique(union([boundVarsFinder(vars, test, bound),
                      boundVarsFinder(vars, dit, bound),
                      boundVarsFinder(vars, dif, bound)], []),
               bound)
      | boundVarsFinder (vars, Abs(_, body), bound) =
        boundVarsFinder (vars, body, bound)
      | boundVarsFinder (vars, AbsOpt(_, _, body), bound) =
        boundVarsFinder (vars, body, bound)
      | boundVarsFinder (vars, AbsVar(_, body), bound) =
        boundVarsFinder (vars, body, bound)
      | boundVarsFinder (vars, App(operator, operands), bound) =
        unique(union(boundVarsFinder(vars, operator, bound) ::
                     (map (fn arg =>
                              boundVarsFinder(vars, arg, bound))
                          operands),
                     bound), bound)
      | boundVarsFinder (vars, AppTP(operator, operands), bound) =
        unique(union(boundVarsFinder(vars, operator, bound) ::
                     (map (fn arg =>
                              boundVarsFinder(vars, arg, bound))
                          operands),
                     bound), bound)
      | boundVarsFinder (vars, Seq(exprs), bound) =
        unique(union((map (fn expr =>
                              boundVarsFinder(vars, expr, bound))
                          exprs), bound), bound)
      | boundVarsFinder (vars, Or(exprs), bound) =
        unique(union((map (fn expr =>
                              boundVarsFinder(vars, expr, bound))
                          exprs), bound), bound)
      | boundVarsFinder (vars, Set(var, value), bound) =
        unique(union([boundVarsFinder(vars, var, bound),
                      boundVarsFinder(vars, value, bound)],
                     bound),
               bound)
      | boundVarsFinder (vars, Def(deffed, value), bound) =
        unique(union([boundVarsFinder(vars, deffed, bound),
                      boundVarsFinder(vars, value, bound)],
                     bound),
               bound)
        
        
    fun boxVars (banged : string list, vars : string list,
                 expr : Expr) =
        let val setList = map (fn var =>
                                  Set(VarParam(var,
                                               placeFinder(var, vars)),
                                      App(VarFree("list"),
                                          [VarParam(var,
                                                    placeFinder(var,
                                                                vars))])))
                              banged
        in
            Seq(setList @ [foldl (fn (var, e) =>
                                     boxVarInExpr(e, var))
                                 expr banged])
        end;

    fun bangFinder (vars : string list, Const(s) : Expr,
                    banged : string list) =
        banged 
      | bangFinder(vars, Set(VarBound(var, _, _), value), banged) =
        (case (inVarList(var, vars)) of
             NONE => bangFinder (vars, value, banged)
           | SOME(var) => var :: (bangFinder(vars, value,
                                             banged)))
      | bangFinder (vars, Set(VarFree(var), value), banged) =
        (case (inVarList(var, vars)) of
             NONE => bangFinder (vars, value, banged)
           | SOME(var) => var :: (bangFinder(vars, value,
                                             banged)))
      | bangFinder(vars, Set(VarParam(var, _), value), banged) =
        (case (inVarList(var, vars)) of
             NONE => bangFinder (vars, value, banged)
           | SOME(var) => var :: (bangFinder(vars, value,
                                             banged)))
      | bangFinder(vars, Set(exp, value), banged) =
        unique(bangFinder(vars, exp, banged),
               bangFinder(vars, value, banged))
      | bangFinder(vars, Seq(exprLst), banged) =
        let val allBangedList = map (fn expr =>
                                        bangFinder(vars, expr,
                                                   []))
                                    exprLst
        in
            unique(union(allBangedList, []), banged)
        end
      | bangFinder (vars, If(test, dit, dif), banged) =
        unique(union((map (fn expr => bangFinder(vars, expr,
                                                 []))
                          [test, dit, dif]), []), banged)
      | bangFinder (vars, Or(exprLst), banged) =
        let val allBangedList = map (fn expr =>
                                        bangFinder(vars, expr,
                                                   []))
                                    exprLst
        in
            unique(union(allBangedList, []), banged)
        end
      | bangFinder (vars, Def(e1, e2), banged) =
        unique(bangFinder(vars, e1, banged),
               bangFinder(vars, e2, banged))
      | bangFinder (vars, Abs(args, body), banged) =
        let val filteredVars = 
                List.filter (fn (x : string) =>
                                not (isSome(inVarList(x, args))))
                            vars
        in
            bangFinder(filteredVars, body, banged)
        end
      | bangFinder (vars, AbsOpt(args, extra, body), banged) =
        let val filteredVars =
                List.filter (fn x : string =>
                                not (isSome(inVarList(x, args))))
                            vars
        in
            bangFinder(filteredVars, body : Expr, banged)
        end
      | bangFinder (vars, AbsVar(argList, body), banged) =
        bangFinder (vars, body, banged)
      | bangFinder (vars, exp as App(operator, operands),
                    banged) =

         let val allBangedList = map (fn expr =>
                                         bangFinder(vars, expr,
                                                    []))
                                     operands
         in
             unique(bangFinder(vars, operator, banged),
                    unique(union(allBangedList, banged), banged))
         end
      | bangFinder (vars, AppTP(operator, operands), banged) =
        let val allBangedList = map (fn expr =>
                                        bangFinder(vars, expr,
                                                   []))
                                    operands
        in
            (unique(bangFinder(vars, operator, banged),
                    unique(union(allBangedList, banged), banged))
             : string list)
        end
      | bangFinder (vars, VarParam(var, i), banged) =
        banged
      | bangFinder (_, VarBound(_), banged) = banged
      | bangFinder (_, VarFree(_), banged) = banged
      | bangFinder (vars, err, banged) =
        raise NotYetImplemented
    and findBangedVars  (vars, body : Expr) =
        intersection(bangFinder(vars, body, [] : string list),
                     boundVarsFinder(vars, body, []), []);
        



        
    fun boxSet' (e as Const(_)) = e
      | boxSet' (e as VarFree(_)) = e
      | boxSet' (e as VarParam(_, _)) = e
      | boxSet' (e as VarBound(_, _, _)) = e
      | boxSet' (Abs(vars, body)) =
        (case findBangedVars(vars, body) of
             [] => Abs(vars, boxSet'(body))
           | bangedvars => Abs(vars, boxVars(bangedvars,
                                             vars,
                                             boxSet'(body))))
      | boxSet' (AbsOpt(vars, opt, body)) =
        (case findBangedVars(vars, body) of
             [] => boxSet' body
           | bangedvars : string list =>
             AbsOpt(vars, opt, boxVars(bangedvars, vars, boxSet'(body)))
        (* | _ => raise AbsOptBoxingException *))
      | boxSet' (AbsVar(var, body)) =
        AbsVar(var, boxSet' body)
      | boxSet' (Seq(exprList)) =
        Seq((map boxSet' exprList))
      | boxSet' (If(e1, e2, e3)) =
        If (boxSet' e1, boxSet' e2, boxSet' e3)
      | boxSet' (App(expr, exprList)) = 
        App(boxSet' expr, (map boxSet' exprList))
      | boxSet' (AppTP(expr, exprList)) =
        AppTP(boxSet' expr, (map boxSet' exprList))
      | boxSet' (Or(exprList)) =
        Or(map boxSet' exprList)
      | boxSet' (Set(e1, e2)) =
        Set(boxSet' e1, boxSet' e2)
      | boxSet' (Def(e1, e2)) =
        Def(boxSet' e1, boxSet' e2)
      (* leave expr as it was - vars and consts *)
      | boxSet' els = els;
in
fun lexicalAddressing expr = lexicalWorker(expr, [[]]);
fun boxSet (expr : Expr) = boxSet'(lexicalAddressing expr);
fun annotateTC expr = run(expr, true);
fun analysis expr =  lexicalAddressing (annotateTC (boxSet expr));
end  
end; (* of struct SemanticAnalysis *)





fun fileToString ( filename : string ) =
    let val f = TextIO . openIn filename
        fun loop s =
            (case TextIO . input1 f
              of NONE => s
               | SOME c => loop (c :: s))
        val result = String . implode (rev (loop []))
    in
        TextIO . closeIn f;
        result
    end;

fun stringToFile (str : string, filename : string) =
    let val f = TextIO.openOut filename
    in
        ( TextIO.output(f, str);
          TextIO.closeOut f )
    end;


    


          
structure ExprMap =
BinaryMapFn (struct
             type ord_key = Expr
             val compare =
                 (fn (e1 : Expr, e2 : Expr) =>
                     String.compare(exprToString (e1),
                                    exprToString (e2)));
             end);

signature CODE_GEN =
sig
    val cg : Expr list -> string ;
    val compileSchemeFile : string * string -> unit;
end;
    
val myExpMap = ExprMap.empty;
val myExpMap = ExprMap.insert(myExpMap, Const(Bool(true)), 1)
val myExpMap = ExprMap.insert(myExpMap, Const(Bool(false)), 3)
val myExpMap = ExprMap.insert(myExpMap, Const(Nil), 5)
val myExpMap = ExprMap.insert(myExpMap, Const(Void), 6)
(* an Expr of type Var is impossible, this is to keep 
reference for the next empty space *)
val myExpMap = ExprMap.insert(myExpMap, Var("next"), 7)

fun insertConst(mp, expr : Expr, sizeof : int) =
    let val next = Var("next")
        val start = ExprMap.lookup(mp, next)
        val withNext = ExprMap.insert(mp, next, start + sizeof)
        val dontcare = print ("insertConst got " ^ exprToString expr ^ "\n")
        val d21 = (case expr of
                       VarFree(v) => print("var free " ^ v ^ "\n")
                     | Const(String(s)) => print("string: " ^ s ^ "\n")
                     | Const(Symbol(s)) => print("symbol: " ^ s ^ "\n")
                     | Const(Pair(s1, s2)) => print ("pair! " ^ exprToString expr)
                     | _ => print("+"))
    in
        if not (isSome (ExprMap.find(withNext, expr)))
        then (print ("insertConst creating " ^ exprToString expr ^ "\n");
              ExprMap.insert(withNext, expr, start))
        else mp                 (* already in, do nothing *)
    end
    
structure CodeGen : CODE_GEN =
struct
local
    exception NotYetImplemented;
    val nl = "\n"
    val t = "  "
    val tt = t ^ t
    val ttt = tt ^ tt
    val docNl = nl ^ ttt ^ ttt ^ tt
    val is_sob_true = "IS_SOB_TRUE"
    val is_sob_void = "IS_SOB_VOID"
    val make_sob_void = "MAKE_SOB_VOID"
    val make_sob_nil = "MAKE_SOB_NIL"
    val write_sob = "WRITE_SOB"
    val end_of_the_world = "END_OF_THE_WORLD"
    (* auto generated code FOR the code generator!!  *)
    val R0 = "R0";  val r0 = "R0"; val R1 = "R1"; val r1 = "R1"
    val R2 = "R2";  val r2 = "R2"; val R3 = "R3"; val r3 = "R3"
    val R4 = "R4";  val r4 = "R4"; val R5 = "R5"; val r5 = "R5"
    val R6 = "R6";  val r6 = "R6"; val R7 = "R7"; val r7 = "R7"
    val R8 = "R8";  val r8 = "R8"; val R9 = "R9"; val r9 = "R9"
    val R10 = "R10"; val r10 = "R10"; val R16 = "R16"; val r16 = "R16"
    val counter = ref 0
    (* again, auto-generated code^2 *)
    val T_CLOSURE = "T_CLOSURE"; val t_closure = "T_CLOSURE"
    val T_VOID = "T_VOID"; val t_void = "T_VOID"
    val T_NIL = "T_NIL"; val t_nil = "T_NIL"
    val T_BOOL = "T_BOOL"; val t_bool = "T_BOOL"
    val T_CHAR = "T_CHAR"; val t_char = "T_CHAR"
    val T_INTEGER = "T_INTEGER"; val t_integer = "T_INTEGER"
    val T_STRING = "T_STRING"; val t_string = "T_STRING"
    val T_SYMBOL = "T_SYMBOL"; val t_symbol = "T_SYMBOL"
    val T_PAIR = "T_PAIR"; val t_pair = "T_PAIR"
    val T_VECTOR = "T_VECTOR"; val t_vector = "T_VECTOR"
    val malloc = "MALLOC"
    val fp = "FP"; val sp = "SP"
    val make_sob_clos = "MAKE_SOB_CLOSURE"
    val make_sob_string = "MAKE_SOB_STRING"
    val make_sob_integer = "MAKE_SOB_INTEGER"
    val make_sob_bool = "MAKE_SOB_BOOL"
    val make_sob_symbol = "MAKE_SOB_SYMBOL"
    val void = Const(Void)
    val putchar = "PUTCHAR"
    fun upCase(str) =
        String.implode(map Char.toUpper (String.explode(str)))
    (* convert to upper case and precede with '_' if not present. *)
    fun upCase'(str) =
        let val s = upCase(str)
        in
            if String.sub(s,0) = #"_" then s else "_" ^ s
        end
    (* inc counter and return it's string *)
    fun tag() =
        (counter := !counter + 1; Int.toString(!counter))
    fun imm(n) = if n >= 0
                 then
                     "IMM(" ^ Int.toString(n) ^ ")"
                 else
                     "IMM(-" ^ Int.toString(abs n) ^ ")"
    fun Imm(s) = "IMM(" ^ s ^ ")"
    fun doc(exp : Expr) = 
        tt ^"/*" ^ nl ^ ttt ^ "code for the expression:" ^ nl ^
        ttt ^ exprToString(exp) ^ nl ^ tt ^ "*/"^ nl
    fun rem(txt) = "/* " ^ txt ^ " */" ^ nl
    fun label(lab, numTag, docString) = " " ^ lab ^ numTag ^ ":" ^ tt ^
                                        "/* " ^ docString ^ " */" ^ nl
    fun drop(n) = t ^ "DROP(" ^ Int.toString(n) ^ ");" ^ nl
    fun drops(s) = t ^ "DROP(" ^ s ^ ");" ^ nl
    fun chrToC (c : char) = "'" ^ (str c) ^ "'";
    fun push(reg : string, docString) =
        t ^ "PUSH(" ^ reg ^ ");" ^ tt ^ rem(docString)
    fun pushn(n : int, docString) =
        push(imm(n), docString)
    fun pushReg(regNum, docString) =
        t ^ "PUSH(R" ^ Int.toString(regNum) ^ ");" ^ tt ^ rem(docString)
    fun pop(reg, docString) =
        t ^ "POP(" ^ reg ^ ");" ^ tt ^ rem(docString)
    fun cmp(calc, docString) =
        t ^ "CMP(" ^ calc ^ ");" ^ tt ^ rem(docString)
    fun call(func, docString) =
        t ^ "CALL(" ^ func ^ ");" ^ tt ^ rem(docString)
    fun calla(addr, docString) =
        t ^ "CALLA(" ^ addr ^ ");" ^ tt ^ rem(docString)
    fun movReg(reg, source, docString) =
        t ^ "MOV(R" ^ Int.toString(reg) ^", " ^ source ^ ");" ^
        tt ^ rem(docString)
    fun mov(target, source, docString) =
        t ^ "MOV(" ^ target ^", " ^ source ^ ");" ^ tt ^ rem(docString)
    fun jump("", target, docString) =
        t ^ "JUMP(" ^ target ^ ");"  ^ tt ^ rem(docString)
      | jump(cond, target, docString) =
        t ^ "JUMP"^ upCase'(cond) ^ "(" ^ target ^ ");" ^ tt ^ rem(docString)
    fun jumpEq(target, docString) =
        jump("_EQ", target, docString)
    (* shorthand for JUMP *)
    fun jmp(target, docString) = jump("", target, docString)
    fun fparg(minor) =
        "FPARG(" ^ Int.toString(minor) ^ ")"
    fun Fparg(str) =
        "FPARG(" ^ str ^ ")"
    fun indd(reg : string, offset) =
        "INDD(" ^ reg ^ ", " ^ Int.toString(offset) ^ ")"
    fun Indd(reg : string, offset : string) =
        "INDD(" ^ reg ^ ", " ^ offset ^ ")"
    fun ind(reg) = "IND(" ^ reg ^ ")"
    fun missing (exp, exprs, run, envR, params, symMap) =
        nl ^ doc(exp) ^ " SHOULD BE HERE, BUT IT'S " ^
        "NOT YET IMPLEMENTED." ^ nl ^ nl ^
        run(exprs, envR, params, symMap)
    fun for(loop, body) =
        t ^ "for(" ^ loop ^") {" ^ nl ^
        tt ^ body ^ 
        t ^ "}" ^ nl
    fun i2s(n) = Int.toString(n)
    val env = fparg(0)
    val ret = "RETURN;" ^ nl
    val CONCLUSION = nl ^ nl ^ rem("end of generated code. EXITING:") ^
                     rem("that was useless..") ^
                     label("END_OF_THE_WORLD", "", "(only as we know it.)") ^
                     "  /* newline and stop machine */\n" ^
                     "  PUSH(IMM('\\n'));\n  CALL(PUTCHAR);\n" ^
                     "  STOP_MACHINE;\n\n  return 0;\n}\n"
    fun prnStr(s) =
        nl ^
        concat(map (fn c => if c <> #"\\"
                            then
                                t ^ "OUT(IMM(2), IMM('" ^ (str c) ^
                                "'));" ^ nl
                            else
                                t ^ "OUT(IMM(2), IMM('" ^ "\\\\" ^
                                "'));" ^ nl)
                   (explode s)) ^
        t ^ "OUT(IMM(2),IMM('\\n'));" ^ nl
    fun handleChars("", newS) = newS
      | handleChars(s, newS) =
        let val c = String.sub(s,0)
            val rest = String.extract(s, 1, NONE)
        in
            if c = #"\\"
            then
                handleChars(rest, newS ^ "\\\\")
            else
                handleChars(rest, newS ^ str c)
        end
    fun testArgNum(exp, n) = 
        let val L_bad_arg_count = "L_BAD_ARG_COUNT" ^ tag()
            val L_arg_count_ok = "L_ARG_COUNT_OK" ^ tag()
        in
            rem("testing arg number for: " ^ exprToString(exp) ^
                "should be " ^ Int.toString(n)) ^
            cmp(fparg(1) ^ ", " ^ imm(n),
                "number of args (taken from fparg(1)," ^
                " n from compiler)") ^
            jumpEq(L_arg_count_ok, "correct number of args to ABS") ^
            label(L_bad_arg_count, "", "") ^
            prnStr("ERROR: bad number of args. oui.") ^
            prnStr("in: " ^ (exprToString exp)) ^
            jmp(end_of_the_world, "exiting due to bad arg count to Abs") ^
            label(L_arg_count_ok,"", "")
        end
        
    fun cLabel(labl) = "LABEL(" ^ labl ^ ")"
    fun lengthSexprs ([], l) = l
      | lengthSexprs (Void :: elts, l) =
        lengthSexprs(elts, l + 1)
      | lengthSexprs (Nil :: elts, l) =
        lengthSexprs(elts, l + 1)
      | lengthSexprs(Pair(s1, s2) :: elts, l) =
        lengthSexprs(s1 :: s2 :: elts, l) 
      | lengthSexprs(Vector(vs) :: elts, l) =
        lengthSexprs(elts, lengthSexprs(vs, l))
      | lengthSexprs(Symbol(s) :: elts, l) =
        lengthSexprs(elts, l + 5)
      | lengthSexprs(String(s) :: elts, l) =
        lengthSexprs(elts, l + 2 + size s)
      | lengthSexprs(Number(n) :: elts, l) =
        lengthSexprs(elts, l + 2)
      | lengthSexprs(Bool(_) :: elts, l) =
        lengthSexprs(elts, l)
      | lengthSexprs(Char(c) :: elts, l) =
        lengthSexprs(elts, l + 2)
        
    fun getCConst(C as Const(sexpr), symMap) =
        (case sexpr of
             Void => symMap
           | Nil => symMap
           | p as Pair(s1, s2) =>
             if isSome (ExprMap.find(symMap, C))
             then
                 symMap
             else
                 (if debug then print("getCConst Pair <" ^ exprToString C ^
                                      "> calling insertConst\n")
                  else print("");
                 insertConst(symMap, C, lengthSexprs([p], 0)))
           | Vector(elts) =>
             if isSome (ExprMap.find(symMap, C))
             then
                 symMap
             else
                 (if debug then print("getCConst Vector calling insertConst\n")
                  else print("");
                 insertConst(symMap, C,
                             lengthSexprs(elts, 0) +
                             2 + length elts))
 (* 2 for vector (T_VECTOR and n), length elts is the *)
 (* actual length of the vector, lengthSexprs(elts) is*)
 (* size of the vector on the heap *)
           | sym as  Symbol(s) =>
             if isSome (ExprMap.find(symMap, C))
             then
                 symMap (* already in symtab *)
             else
                 (* inserting the string first, then the symbol *)
                 (* (creating the string as well) *)
                 (if debug then print("getCConst Symbol <" ^ exprToString C ^
                                      "> calling insertConst\n")
                  else print("");
                  insertConst(insertConst(symMap, Const(String(s)),
                                          2 + size s),
                              C, lengthSexprs([sym], 0)))
           | st as String(s) =>
             if isSome(ExprMap.find(symMap, C))
             then
                 symMap
             else
                 insertConst(symMap, C, lengthSexprs([st], 0))
           | Number(n) =>
             if isSome (ExprMap.find(symMap, C))
             then
                 symMap
             else
                 insertConst(symMap, C, 2)
           | Bool(_) => symMap
           | Char(c) =>
             if isSome (ExprMap.find(symMap, C))
             then
                 symMap
             else
                 insertConst(symMap, C, 2))
      | getCConst(_, symMap) = symMap
        
    fun getCVar(V as VarFree(name), symMap) =
        if isSome (ExprMap.find(symMap, V))
        then
            symMap
        else
            (print "\n\ngetCVar calling insertConst\n";
             let val withString =
                     insertConst(symMap, Const(String(name)),
                                 lengthSexprs([String(name)], 0))
             in
                 insertConst(withString, Const(Symbol(name)),
                             lengthSexprs([Symbol(name)], 0))
             end)
      | getCVar(_, symMap) = symMap
                             
    fun getConsts([], symMap) = symMap
      | getConsts((C as Const(sexpr)) :: exprs, symMap) =
        getConsts(exprs, getCConst(C, symMap))
      | getConsts((V as VarFree(v)) :: exprs, symMap) =
        getConsts(exprs, getCVar(V, symMap))
      | getConsts(If(test, dit, dif) :: exprs, symMap) =
        getConsts(exprs, getConsts([test, dit, dif], symMap))
      | getConsts(Abs(vars, body) :: exprs, symMap) =
        getConsts(exprs, getConsts([body], symMap))
      | getConsts(AbsOpt(vars, extra, body) :: exprs, symMap) = 
        getConsts(exprs, getConsts([body], symMap))
      | getConsts(AbsVar(v, body) :: exprs, symMap) =
        getConsts(exprs, getConsts([body], symMap))
      | getConsts(App(operator, operands) :: exprs, symMap) =
        getConsts(exprs, getConsts(operator :: operands, symMap))
      | getConsts(AppTP(operator, operands) :: exprs, symMap) =
        getConsts(exprs, getConsts(operator :: operands, symMap))
      | getConsts(Seq(exprList) :: exprs, symMap) =
        getConsts(exprs, getConsts(exprList, symMap))
      | getConsts(Or(exprList) :: exprs, symMap) =
        getConsts(exprs, getConsts(exprList, symMap))
      | getConsts(Set(var, value) :: exprs, symMap) =
        getConsts(exprs, getConsts([var, value], symMap))
      | getConsts(Def(var, value) :: exprs, symMap) =
        getConsts(exprs, getConsts([var, value], symMap))
      | getConsts(VarBound(_,_,_) :: exprs, symMap) = 
        getConsts(exprs, symMap)
      | getConsts(VarParam(_,_) :: exprs, symMap) =
        getConsts(exprs, symMap)
      | getConsts(Var(_) :: exprs, symMap) =
    (* bad lexical analysis. never mind ("production"" code) *)
        getConsts(exprs, symMap)
    fun createNumber(n, loc) =
        pushn(n, "") ^
        call(make_sob_integer, "creating " ^ i2s(n) ^
                               " should be at " ^ i2s(loc)) ^
        drop(1)
    fun createChar(c, loc) =
        pushn(ord c, "pushing " ^ (str c) ^ "'s ord value.") ^
        call("MAKE_SOB_CHAR", "should be at " ^ i2s(loc)) ^
        drop(1)
    fun createBool(b : bool, loc) =
        (if b
         then pushn(1, "1 to create SOB_BOOL_TRUE")
         else pushn(0, "0 to create SOB_BOOL_FALSE")) ^
        call(make_sob_bool, "creating boolean. should be at 1/3. really at "
                            ^ i2s(loc)) ^
        drop(1)
    fun createString(exp, s, loc) =
        doc(exp) ^
        concat(map (fn c => push((chrToC c), "to make a string")) (explode s)) ^
        pushn(size s, "string's length") ^
        call(make_sob_string, "creating the string") ^
        drop(size s + 1)
    fun createSymbol(s, loc, symMap) =
        rem("creating symbol: " ^ s) ^
        (if isSome (ExprMap.find(symMap, Const(String(s))))
         (* string already exists *)
         then
             mov(R0, imm(ExprMap.lookup(symMap, Const(String(s)))),
                 "ptr to the string \"" ^ s ^ "\" from lookup")
         else
             rem("first, the name.") ^
             createString(Const(String(s)), s, loc)) ^
        call(make_sob_symbol,"now creating the symbol")
    fun createVector(v, loc) = "missing vector creation!!"
    fun createPair(p, loc) = "missing pair creation!!"
    fun genConst(C as Const(sexpr), symMap) =
        let val optLoc = ExprMap.find(symMap, C)
            val loc = if isSome(optLoc) then valOf optLoc
                      else 9998877
        in
            (case sexpr of
                 Void => call(make_sob_void, "create void. is at:" ^
                                             i2s(loc) ^ " wanted: 6. CHECK!")
               | Nil => call(make_sob_nil, "create nil. should be at 5. is at:" ^
                                           "mem[" ^ i2s(loc) ^ "]")
               | p as Pair(_,_) => createPair(p, loc)
               | v as Vector(elts) => createVector(v, loc)
               | Symbol(s) => createSymbol(s, loc, symMap)
               | String(s) => createString(C, s, loc)
               | Number(n : int) => createNumber(n, loc)
               | Bool(b) => createBool(b, loc)
               | Char(c) => createChar(c, loc))
        end
      | genConst(nxt as Var("next"), symMap) =
        rem("next available place in mem should be " ^
            i2s(ExprMap.lookup(symMap, nxt)))
      | genConst(expr : Expr, symMap) = rem("missing genConst for: " ^
                                            exprToString expr)
    fun generateConstants(symMap) =
        let val exprs = ExprMap.listItemsi symMap
            val sorted = ListMergeSort.sort (fn (a, b) => (#2 a) > (#2 b)) exprs
        in
            concat(map (fn exp => genConst(exp, symMap))
                       (map (fn t => (#1 t)) sorted))
        end
    fun prnR0() =
        let val L_after_print = "L_AFTER_PRINT" ^ tag()
        in
            rem("End of Sexpr, printing R0 - if not <Void>") ^
            movReg(1, "R0", "save the result in R1") ^
            pushReg(1, "pushing R1 for void-check") ^
            call(is_sob_void, "if R0 is #Void, don't print") ^
            drop(1) ^
            cmp("R0, "^ imm(1), "1 means R0 was void, 0 means" ^
                                " it wasn't") ^
            jumpEq(L_after_print, "do not print") ^
            pushReg(1, "push R1 for print") ^
            call(write_sob, "print the result before going to next Sexpr") ^
            drop(1) ^
            label(L_after_print, "", "") ^
            t ^ "PUSH(IMM('\\n'))" ^ nl ^
            call(putchar, "print the newline") ^
            drop(1)
        end
    fun lookupConst (C, symMap) =
        rem("inside lookupConst. looking for " ^ exprToString C) ^
        (case (ExprMap.find(symMap, C)) of
             (* no var creation yet.. FIXME *)
             NONE => exprToString C ^ " FIXME: SHOULD BE HERE, BUT IT'S " ^
                     "NOT YET IMPLEMENTED." ^ nl ^ nl
           | SOME(loc) =>
             mov(R0, i2s loc, "the address of " ^ exprToString C ^
                              "(should be) moved to R0"))
        
                  
    fun run ([], _, _, _) = ""
      | run ((exp as VarParam(name, minor)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        movReg(0, fparg(2 + minor), "move param var " ^ name ^ " to R0") ^
        run(exprs, envR, params, symMap)
      | run ((exp as VarBound(name, major, minor)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        mov(R0, fparg(0), "R0 <- env from FPARG(0)") ^
        mov(R0, indd(R0, major), R0 ^ " <- env[major]") ^
        mov(R0, indd(R0, minor), R0 ^ " <- env[major][minor]") ^
        run(exprs, envR, params, symMap)
      | run ((exp as VarFree(name)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        (case (ExprMap.find(symMap, Const(Symbol(name)))) of
             (* no var creation yet..*)
             NONE => exprToString exp ^ " SHOULD BE HERE, BUT IT'S " ^
                     "NOT YET IMPLEMENTED." ^ nl ^ nl
           | SOME(loc) =>
             mov(R0, i2s loc, "the address of " ^ exprToString exp ^
                              " (should be) moved to R0")) ^
        run(exprs, envR, params, symMap)
      | run ((exp as Set(VarParam(name, major), value)) :: exprs,
            envR, params, symMap) =
        doc(exp) ^
        rem("calculating the value of " ^ exprToString(value)) ^
        run([value], envR, params, symMap) ^
        mov(fparg(2 + major), "R0", "setting FPARG(major + 2) from R0") ^
        rem("R0 <- #Void after set.") ^
        run([Const(Void)], envR, params, symMap) ^
        run(exprs, envR, params, symMap)
      | run ((exp as Set(VarBound(name, major, minor), value)) :: exprs,
            envR, params, symMap) =
        doc(exp) ^
        rem("calculating the value of " ^ exprToString(value)) ^
        run([value], envR, params, symMap) ^
        mov(R1, env, "R1 <- env") ^
        mov(R1, indd(R1, major), "R1 <- env[major]") ^
        mov(indd(R1, minor), R0, "env[major][minor] <- R0") ^
        rem("R0 <- #Void after set.") ^
        run([Const(Void)], envR, params, symMap) ^
        run(exprs, envR, params, symMap)
      | run ((exp as Set(VarFree(name), value)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        push(R10, "we'll save the value of \"value\" in R10") ^
        run([value], envR, params, symMap) ^
        mov(R10, R0, "save the value of \"value\" in R10") ^
        lookupConst(Const(Symbol(name)), symMap) ^
        mov(ind(R0), R10, "assign \"value\" from R10 to " ^ name) ^
        pop(R10, "") ^
        run(exprs, envR, params, symMap)
        (* missing(exp, exprs, run, envR, params, symMap) *)
      | run ((exp as AppTP(FIX, ME)) :: exprs, envR, params, symMap) =
        (print "FIX AppTP!!!!\n";
         run(App(FIX, ME) :: exprs, envR, params, symMap))
      | run ((exp as App(operator, operands)) :: exprs, envR, params, symMap) =
        let val L_NOT_CLOSURE = "L_NOT_CLOSURE" ^ tag()
            val L_IS_CLOSURE = "L_IS_CLOSURE" ^ tag()
            val n_clos_tag = tag()
        in
            doc(exp) ^
            concat(map (fn operand => run([operand], envR, params, symMap) ^
                                      push(R0, "pushing the result as" ^
                                               " an arg for" ^
                                               docNl ^ "the upcoming call to " ^
                                               exprToString(operator)))
                       (rev operands)) ^
            push(Int.toString(length operands), "pushing (n) - the number" ^
                                                " of args for the " ^
                                                "application") ^
            rem("now executing proc/operator BEFORE it's application:") ^
            run([operator], envR, params, symMap) ^
            cmp(ind(R0) ^ ", " ^ Imm(T_CLOSURE), "R0 must be of type" ^ docNl ^
                                                 " closure to be applied") ^
            jump("_EQ", L_IS_CLOSURE, "only a closure can be applied as" ^ docNl
                                      ^ "an operator. this is a run-time check")
            ^
            label(L_NOT_CLOSURE, "", "a non-closure SOB was (almost) applied." ^
                                     docNl ^ "print and goto end") ^
            rem("printing N-CLOS"^ n_clos_tag ^ " to screen") ^
            t ^ "OUT(IMM(2)," ^ Imm("'N'") ^ ");" ^ nl ^
            t ^ "OUT(IMM(2)," ^ Imm("'-'") ^ ");" ^ nl ^
            t ^ "OUT(IMM(2)," ^ Imm("'C'") ^ ");" ^ nl ^
            t ^ "OUT(IMM(2)," ^ Imm("'L'") ^ ");" ^ nl ^
            t ^ "OUT(IMM(2)," ^ Imm("'O'") ^ ");" ^ nl ^
            t ^ "OUT(IMM(2)," ^ Imm("'S'") ^ ");" ^ nl ^
            prnStr(" came from N-CLOS" ^ n_clos_tag) ^
            t ^ "OUT(IMM(2)," ^ Imm("'\\n'") ^ ");" ^ nl ^
            jmp("END_OF_THE_WORLD", "non-closure application. stop the execution") ^
            label(L_IS_CLOSURE, "", "we didn't crash - it's a closure after all.") ^
            push(indd(R0, 1), "pushing env") ^
            calla(indd(R0, 2), "calling the proc (code) part of R0") ^
            drops("2 + STARG(0)") ^
            rem("dropping n + 2 args, according to what's on the stack now,"
                ^ nl ^ "as shown in class. IS STARG(0) THE RIGHT CHOICE??") ^
            run(exprs, envR, params, symMap)
        end
      | run ((exp as Abs(parameters, body)) :: exprs, envR, params, symMap) =
        let val L_actual_clos = "L_ACTUAL_CLOS" ^ tag()
            val L_cont = "L_CONT" ^ tag()
        in
            doc(exp) ^
            mov(R1, env, "R1 <- env") ^
            push(imm(length(envR) + 1), "pushing |env| + 1 for malloc") ^
            call(malloc, "calling malloc to make room for the extended env") ^
            drop(1) ^
            mov(R2, R0, "R2 <- R0. R2 is the pointer to the new env") ^
            for("_i=0, _j=1; _i<" ^ Int.toString(length(envR)) ^ "; ++_i, ++_j",
                mov(Indd(R2, "_j"), Indd(R1, "_i"), "R2[_j] <- R1[_i]")) ^ 
            push(imm(length(parameters)), "pushing |parameters| for malloc") ^
            call(malloc, "calling malloc to make room for the params in env") ^
            drop(1) ^
            mov(ind(R2), R0, "R2[0] <- R0 from malloc") ^
            for("_i=0; _i<" ^ Int.toString(length parameters) ^ ";_i++",
                mov(R3, ind(R2), "R3 <- R2[0]") ^ tt ^
                mov(Indd(R3, "_i"), Fparg("2 + _i"),
                         "R2[0][_i] <- FPARG(2 + _i)")) ^
            jmp(L_cont, "the actual code for the closure" ^
                        " will be jumped over.") ^
            label(L_actual_clos, "",
                  "the closure code to be executed on applic") ^
            push(fp, "") ^
            mov(fp, sp, "") ^
            testArgNum(exp, length parameters) ^
            run([body], parameters :: envR, params, symMap) ^
            pop(fp, "") ^
            ret ^
            label(L_cont, "", "prepare the closure IN COMPILE TIME") ^
            (* push code; push env; call make_sob_clos *)
            push(cLabel(L_actual_clos), "pushing code part of the closure") ^
            push(R2, "R2 is the pointer to env (runtime)") ^
            call(make_sob_clos, "create the closure") ^
            drop(2) ^
            nl ^rem("END of Abs:" ^  exprToString(exp)) ^ nl ^ nl ^
            run(exprs, parameters :: envR, params, symMap)
        end
      | run ((exp as If(test, dit, dif)) :: exprs, envR, params, symMap) =
        let val tag1 = tag()
        in
            doc(exp) ^
            run([test], envR, params, symMap) ^
            pushReg(0, "pushing R0 to check if it's true") ^
            call(is_sob_true, "test if R0 is true") ^
            drop(1) ^
            cmp("R0, "^ imm(0), "1 means R0 was true, 0 means it was #f") ^
            jumpEq("Lelse" ^ tag1, "goto dif clause") ^
            rem("if's dit clause:") ^
            run([dit], envR, params, symMap) ^
            jump("", "Lexit" ^ tag1, "goto end of if") ^ 
            label("Lelse", tag1, "if's dif clause") ^ 
            run([dif], envR, params, symMap) ^
            label("Lexit", tag1, "end of if") ^
            run(exprs, envR, params, symMap)
        end
      | run((exp as Const(Void)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        movReg(0, imm(6), "assign #Void to R0") ^
        run(exprs, envR, params, symMap)
      | run((exp as Const(Nil)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        movReg(0, imm(5), "assign nil to R0") ^
        run(exprs, envR, params, symMap)
      | run((exp as Const(Bool(b))) :: exprs, envR, params, symMap) =
        let val boolCode =
                if b then
                    movReg(0, imm(1), "assigning true to R0")
                else
                    movReg(0, imm(3), "assigning false to R0")
        in
            doc(exp) ^
            boolCode ^
            run(exprs, envR, params, symMap)
        end
      | run((exp as Const(String(s))) :: exprs, envR, params, symMap) =
        doc(exp) ^
        lookupConst(exp, symMap) ^
        (* createString(exp, s, ~1000) ^ *)
        (* concat(map (fn c => push((chrToC c), "to make a string")) (explode s)) ^ *)
        (* pushn(size s, "string's length") ^ *)
        (* call(make_sob_string, "creating the string") ^ *)
        (* drop(size s + 1) ^ *)
        run(exprs, envR, params, symMap)
      | run((exp as Const(Char(c))) :: exprs, envR, params, symMap) =
        doc(exp) ^
        pushn(ord c, "pushing " ^ (str c) ^ "'s ord value") ^
        call("MAKE_SOB_CHAR", "") ^
        drop(1) ^
        run(exprs, envR, params, symMap)
      | run((exp as Const(Number(n))) :: exprs, envR, params, symMap) =
        doc(exp) ^
        lookupConst(exp, symMap) ^
        (* pushn(n, "pushing integer value") ^ *)
        (* call("MAKE_SOB_INTEGER", "assiging a number to R0") ^ *)
        (* drop(1) ^ *)
        run(exprs, envR, params, symMap)
      | run((exp as Const(Symbol(name))) :: exprs, envR, params, symMap) =
        doc(exp) ^
        lookupConst(exp, symMap) ^
        run(exprs, envR, params, symMap)
      | run((exp as Const(_)) :: exprs, envR, params, symMap) =
        (* vector, pair *)
        missing(exp, exprs, run, envR, params, symMap)
      | run((exp as Seq(expList)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        run(expList, envR, params, symMap) ^
        run(exprs, envR, params, symMap)
      | run ((exp as Or(expList)) :: exprs, envR, params, symMap) =
        let val endLabel = "L_END_OR" ^ tag()
            val test = movReg(1, "R0", "save the result in R1") ^
                       pushReg(0, "push R0 for falsity test") ^
                       call(is_sob_true, "test R0") ^
                       cmp("R0, " ^ imm(1), "1 means R0 was true." ^
                                            " GOTO end of or") ^
                       drop(1) ^
                       movReg(0, "R1", "restore the result to R0" ^ docNl ^
                                       " (in case we end here.)") ^
                       jumpEq(endLabel, "if this was true, goto end")
            (* this Or is not empty - it was replaced with #f in 
            the lexical analysis *)
            val revExpList = rev expList
            val last = hd(revExpList)
            val butLastExpList = rev(tl revExpList)
        in
            doc(exp) ^
            (foldr (fn (a, b) => a ^ b) ""
                   (map (fn e => run([e], envR, params, symMap) ^ test)
                        butLastExpList)) ^
            run ([last], envR, params, symMap) ^
            label(endLabel, "", "end of Or expression. result is in R0") ^
            run(exprs, envR, params, symMap)
        end
      | run ((exp as Def(var, value)) :: exprs, envR, params, symMap) =
        doc(exp) ^
        push(R1, "R1 will hold the value to be assigned") ^
        rem("now calculating the value to be assigned") ^
        run([value], envR, params, symMap) ^
        mov(R1, R0, "save the result in R0") ^
        rem("now calculating the var to be assigned to: " ^ exprToString var) ^
        run([var], envR, params, symMap) ^
        mov(indd(R0, 2), imm(1), "this variable is now set(1)") ^
        mov(indd(R0, 3), R1, "assiging the value from R1 to the value field R0[3]") ^
        pop(R1, "return R1 from the stack") ^
        rem("setting R0 to Void after Define") ^
        lookupConst(void, symMap) ^
        rem("end of Define expression: " ^ exprToString exp) ^
        run(exprs, envR, params, symMap)
      | run (exp :: exprs, envR, params, symMap) =
        missing(exp, exprs, run, envR, params, symMap)
    val GE = ref myExpMap
  in
fun cg e =
    let val symMap = !GE (* getConsts(e, myExpMap) *)
        (* val dc = print ("CONSTANTS: " ^ constantsCode) *)
    in
        run (e, [], [], symMap)
    end
fun compileSchemeFile (sourceFile, targetFile) =
    (print "TODO: IMPORTANT: RETURN RAM_SIZE AND STACK_SIZE TO 1 MEGA\n";
     let val input = fileToString(sourceFile)
         val exprs =
             map SemanticAnalysis.analysis (TagParser.stringToPEs(input))
         val symMap = getConsts(exprs, myExpMap)
         val sideEffect = (GE := symMap)
         val constantsCode = generateConstants(symMap)
         val program = concat(map (fn exp => (cg [exp]) ^ prnR0()) exprs)
         (* val DEBUG = print("Compiling:" ^ nl ^ input ^ nl) *)
     in
         stringToFile(fileToString("Intro.c") ^
                      constantsCode ^
                      program ^
                      CONCLUSION,
                      targetFile)
     end
    )
end; (* of locals *)
end; (* of structure Code_gen *)
          

