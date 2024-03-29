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
    val boxSet : Expr -> Expr;
    val annotateTC : Expr -> Expr;
    val lexicalAddressing : Expr -> Expr;
    val analysis : Expr -> Expr;
end;

exception NotYetImplemented ;
exception AbsBoxingException;
exception AbsOptBoxingException;
exception AbsVarBoxingException;
          

structure SemanticAnalysis : SEMANTIC_ANALYSIS =
struct
fun inVarList (var, []) = NONE
  | inVarList (var, h::tl) =
    if var = h then SOME(var)
    else
        inVarList(var, tl);
    
fun bangFinder(vars, Const(s), banged) = banged
  | bangFinder(vars, Set(VarBound(var, _, _), value), banged) =
    (case (inVarList(var, vars)) of
         NONE => banged
       | SOME(var) => var :: banged)
  | bangFinder(vars, Seq(exprLst), banged) =
    raise NotYetImplemented
  | bangFinder _ = raise NotYetImplemented;

fun findBangedVars (vars, body) =
    bangFinder(vars, body, []);
    
fun boxVars _ = raise NotYetImplemented;

fun boxSet (origExpr as Abs(vars, body)) =
    (case findBangedVars(vars, body) of
         [] => origExpr
       | [bangedvars] => boxVars(bangedvars, boxSet(body))
       | _ => raise AbsBoxingException)
  | boxSet (origExpr as AbsOpt(vars, opt, body)) =
    (case findBangedVars(vars, body) of
         [] => origExpr
       | [bangedvars] => boxVars(bangedvars, boxSet(body))
       | _ => raise AbsOptBoxingException)
  | boxSet (AbsVar(var, body)) =
    AbsVar(var, boxSet body)
  | boxSet (Seq(exprList)) =
    Seq((map boxSet exprList))
  | boxSet (If(e1, e2, e3)) =
    If (boxSet e1, boxSet e2, boxSet e3)
  | boxSet (App(expr, exprList)) =
    App(boxSet expr, (map boxSet exprList))
  | boxSet (AppTP(expr, exprList)) =
    AppTP(boxSet expr, (map boxSet exprList))
  | boxSet (Or(exprList)) =
    Or(map boxSet exprList)
  | boxSet (Set(e1, e2)) =
    Set(boxSet e1, boxSet e2)
  | boxSet (Def(e1, e2)) =
    Def(boxSet e1, boxSet e2)
  | boxSet els = els ;  (* leave expr as it was - vars and consts *)
    
fun annotateTC _ = raise NotYetImplemented ;
fun lexicalAddressing _ = raise NotYetImplemented;
fun analysis expr = lexicalAddressing (annotateTC (boxSet expr));
    
end; (* of struct SemanticAnalysis *)







