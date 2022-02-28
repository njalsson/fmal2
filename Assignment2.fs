// T-501-FMAL, Spring 2022, Assignment 2

(*
Friðrik Njálsson
Daníel Freyr Gylfason

*)

module Assignment2


type pattern =
    | PUnderscore
    | PVar of string
    | PPair of pattern * pattern

type expr =
    | Var of string
    | Let of pattern * expr * expr
    | Pair of expr * expr
    | Num of int
    | Plus of expr * expr
    | Times of expr * expr

let paren b s = if b then  "(" + s + ")" else s

let prettyprint (e : expr) : string =
    let rec prettyprintPattern (p : pattern) (acc : int) : string =
        match p with
        | PUnderscore -> "_"
        | PVar x -> x
        | PPair (p1, p2) -> paren (1 <= acc) (prettyprintPattern p1 1 + ", " + prettyprintPattern p2 1)
    let rec prettyprintExpr (e : expr) (acc : int) : string =
        match e with
        | Var x -> x
        | Let (p, erhs, ebody) ->
             paren (3 <= acc) ("let " + prettyprintPattern p 0 + " = " + prettyprintExpr erhs 2 + " in " + prettyprintExpr ebody 2)
        | Pair (e1, e2) ->
             paren (3 <= acc) (prettyprintExpr e1 3 + ", " + prettyprintExpr e2 3)
        | Num i -> string i
        | Plus (e1, e2) ->
             paren (4 <= acc) (prettyprintExpr e1 3 + " + " + prettyprintExpr e2 4)
        | Times (e1, e2) ->
             paren (7 <= acc) (prettyprintExpr e1 6 + " * " + prettyprintExpr e2 7)
    prettyprintExpr e 0

let rec varsInPattern (p : pattern) : string list =
    match p with
    | PUnderscore -> []
    | PVar x -> [x]
    | PPair (p1, p2) -> varsInPattern p1 @ varsInPattern p2

let freeVars (e : expr) : string list =
    let rec freeVars' e bound =
        match e with
        | Var x -> if List.exists (fun y -> x = y) bound then [] else [x]
        | Let (p, erhs, ebody) ->
            freeVars' erhs bound @ freeVars' ebody (varsInPattern p @ bound)
        | Pair (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
        | Num _ -> []
        | Plus (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
        | Times (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
    freeVars' e []

let freshVar (root : string) (used : string list) : string =
    let rec freshVar' counter =
        let candidate = if counter = 0 then root else sprintf "%s%i" root counter
        if List.exists (fun x -> candidate = x) used
        then freshVar' (counter + 1)
        else candidate
    freshVar' 0


let rec checkPattern (p : pattern) : bool =
    let rec checkPattern' seen p =
        match p with
        | PUnderscore -> Some seen
        | PVar x -> if List.exists (fun y -> x = y) seen then None else Some (x :: seen)
        | PPair (p1, p2) ->
            match checkPattern' seen p1 with
            | None -> None
            | Some seen -> checkPattern' seen p2
    checkPattern' [] p <> None


////////////////////////////////////////////////////////////////////////
// Problem 1                                                          //
////////////////////////////////////////////////////////////////////////
// type expr =
//     | Var of string
//     | Let of pattern * expr * expr
//     | Pair of expr * expr
//     | Num of int
//     | Plus of expr * expr
//     | Times of expr * expr
// (Write the function checkAllPatterns.)
//  In F#, no variable can appear more than once in a single pattern. For example, the expression
//          let x, (_, x) = (1, (2, 3)) in 4
// is not allowed, but
//          let x, y = (1, (2, 3)) in let (_, x) = y in 4
// is OK. Write a function checkAllPatterns : expr -> bool that takes an expression, 
// and returns true if this rule is followed, false if it is not. 
// You can use checkPattern : pattern -> bool to do this.
let rec checkAllPatterns (e : expr)= 
    match e with
    | Let(x,y,z) -> checkPattern(x) && checkAllPatterns(y) && checkAllPatterns(z)
    | Pair(x,y) -> x = y || checkAllPatterns(x) && checkAllPatterns(y) 
    | _ -> true


type token =
    | NAME of string
    | LET | EQUAL | IN
    | INT of int
    | PLUS | TIMES
    | LPAR | RPAR
    | COMMA | UNDERSCORE
    | ERROR of char


let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)
let isDigit c = '0' <= c && c <= '9'
let digit2Int (c : char) = int c - int '0'
let isLowercaseLetter c = 'a' <= c && c <= 'z'
let isUppercaseLetter c = 'A' <= c && c <= 'Z'
let isLetter c = isLowercaseLetter c || isUppercaseLetter c
let word2Token (s : string) : token =
    match s with
    | "let" -> LET
    | "in"  -> IN
    | _     -> NAME s



////////////////////////////////////////////////////////////////////////
// Problem 2                                                          //
////////////////////////////////////////////////////////////////////////

// (Modify the function tokenize to handle commas and underscores.) 

let rec tokenize (cs : char list) : token list =
    match cs with
    | [] -> []
    | '+'::cs  -> PLUS :: tokenize cs
    | '*'::cs  -> TIMES :: tokenize cs
    | '='::cs  -> EQUAL :: tokenize cs
    | ' '::cs  -> tokenize cs
    | '\t'::cs -> tokenize cs
    | '\n'::cs -> tokenize cs
    | '('::cs  -> LPAR :: tokenize cs
    | ')'::cs  -> RPAR :: tokenize cs
    | ','::cs  -> COMMA :: tokenize cs
    | '_'::cs  -> UNDERSCORE :: tokenize cs
    | c::cs when isDigit c -> tokenizeInt cs (digit2Int c)
    | c::cs when isLowercaseLetter c -> tokenizeWord cs (string c)
    | c::cs -> ERROR c :: tokenize cs
and tokenizeInt cs (acc : int) =
    match cs with
    | c::cs when isDigit c -> tokenizeInt cs (acc * 10 + digit2Int c)
    | _ -> INT acc :: tokenize cs
and tokenizeWord cs (acc : string) =
    match cs with
    | c::cs when isLetter c || isDigit c -> tokenizeWord cs (acc + string c)
    | _ -> word2Token acc :: tokenize cs

let lex s = tokenize (string2Chars s)



////////////////////////////////////////////////////////////////////////
// Problem 3                                                          //
////////////////////////////////////////////////////////////////////////
// Assignment2.fs also contains a broken implementation of a parser, 
// which will fail to parse a let binding in which the pattern is not just a variable. 
// Change the implementations of parsePattern and parseSimplePattern so that the other forms of pattern 
// (pairs and the underscore) are parsed correctly. Commas are non-associative, 
// so parsing of let x, y, z = w in 2 should fail, but let (x, y), z = w in 2 and let x, (y, z) = w in 2 are OK.
// (Modify the functions parsePattern and parseSimplePattern
// to handle a prefix of the token list corresponding to a pair resp.
// the underscore.)
let rec parseExpr (ts : token list) : expr * token list =
    let e1, ts = parseSum ts
    match ts with
    | COMMA :: ts ->
        let e2, ts = parseSum ts
        Pair (e1, e2), ts
    | _ -> e1, ts
and parseSum (ts : token list) : expr * token list =
    let e1, ts = parseSummand ts
    match ts with
    | PLUS :: ts ->
        let e2, ts = parseSum ts
        Plus (e1, e2), ts
    | _ -> e1, ts
and parseSummand (ts : token list) : expr * token list =
    let e1, ts = parseFactor ts
    match ts with
    | TIMES :: ts ->
        let e2, ts = parseSummand ts
        Times (e1, e2), ts
    | _ -> e1, ts
and parseFactor (ts : token list) : expr * token list =
    match ts with
    | NAME x :: ts -> (Var x, ts)
    | LET :: ts ->
        let p, ts = parsePattern ts
        match ts with
        | EQUAL :: ts ->
            let (erhs, ts) = parseExpr ts
            match ts with
            | IN :: ts ->
                let ebody, ts = parseExpr ts
                Let (p, erhs, ebody), ts
            | _ -> failwith "let without in"
        | _ -> failwith "let without equals sign"
    | INT i :: ts -> Num i, ts
    | LPAR :: ts ->
        let e, ts = parseExpr ts
        match ts with
        | RPAR :: ts -> e, ts
        | _ -> failwith "left paren without right paren"
    | _  -> failwith "not a factor"
and parsePattern (ts : token list) : pattern * token list =
    parseSimplePattern ts
and parseSimplePattern (ts : token list) : pattern * token list =
    match ts with
    | NAME x :: ts -> PVar x, ts
    | LPAR :: ts ->
        let p, ts = parsePattern ts
        match ts with
        | RPAR :: ts -> p, ts
        | _ -> failwith "left paren without right paren"
    | LET :: ts -> parsePattern ts
    | EQUAL :: ts -> parsePattern ts
    | _  -> failwith "not a pattern"


let parse (ts : token list) : expr =
    let e, ts = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"
let lexParse (s : string) : expr = parse (lex s)

// parse [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3];;

// parse [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3];;





type value =
    | VPair of value * value
    | VNum of int
type envir = (string * value) list

let rec lookup x env =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env
let rec erase x env =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then env else (y, v) :: erase x env

let addValues (v1 : value) (v2 : value) =
  match v1, v2 with
  | VNum i1 , VNum i2 -> VNum (i1 + i2)
  | _ -> failwith "can only add numbers"
let mulValues (v1 : value) (v2 : value) =
  match v1, v2 with
  | VNum i1 , VNum i2 -> VNum (i1 * i2)
  | _ -> failwith "can only multiply numbers"



////////////////////////////////////////////////////////////////////////
// Problem 4                                                          //
////////////////////////////////////////////////////////////////////////

// (Write the function patternMatch.)

let rec patternMatch (p : pattern) (v : value) (env : envir) : envir =
    failwith "Not implemented"

// (Complete the function eval.)

let rec eval (e : expr) (env : envir) : value =
    match e with
    | Var x -> lookup x env
    | Let (p, erhs, ebody) -> failwith "Not implemented"
    | Pair (e1, e2) -> VPair (eval e1 env, eval e2 env)
    | Num i -> VNum i
    | Plus (e1, e2) -> addValues (eval e1 env) (eval e2 env)
    | Times (e1, e2) -> mulValues (eval e1 env) (eval e2 env)

let run e = eval e []




type nexpr =
    | NVar of string
    | NLet of string * nexpr * nexpr
    | NPair of nexpr * nexpr
    | NFst of nexpr
    | NSnd of nexpr
    | NNum of int
    | NPlus of nexpr * nexpr
    | NTimes of nexpr * nexpr

let nprettyprint (e : nexpr) : string =
    let rec nprettyprintExpr (e : nexpr) (acc : int) : string =
        match e with
        | NVar x -> x
        | NLet (x, erhs, ebody) ->
             paren (3 <= acc) ("let " + x + " = " + nprettyprintExpr erhs 2 + " in " + nprettyprintExpr ebody 2)
        | NPair (e1, e2) ->
             paren (3 <= acc) (nprettyprintExpr e1 3 + " , " + nprettyprintExpr e2 3)
        | NNum i -> string i
        | NPlus (e1, e2) ->
             paren (4 <= acc) (nprettyprintExpr e1 3 + " + " + nprettyprintExpr e2 4)
        | NTimes (e1, e2) ->
             paren (7 <= acc) (nprettyprintExpr e1 6 + " * " + nprettyprintExpr e2 7)
        | NFst e -> paren (8 <= acc) ("fst " + nprettyprintExpr e 8)
        | NSnd e -> paren (8 <= acc) ("snd " + nprettyprintExpr e 8)
    nprettyprintExpr e 0

let rec neval (e : nexpr) (env : envir) : value =
    match e with
    | NVar x -> lookup x env
    | NLet (x, erhs, ebody) ->
         let xval = neval erhs env
         let env1 = (x , xval) :: env
         neval ebody env1
    | NPair (e1, e2) -> VPair (neval e1 env, neval e2 env)
    | NFst e ->
        match neval e env with
        | VPair (v1, v2) -> v1
        | _ -> failwith "expected a pair"
    | NSnd e ->
        match neval e env with
        | VPair (v1, v2) -> v2
        | _ -> failwith "expected a pair"
    | NNum i -> VNum i
    | NPlus (e1, e2) -> addValues (neval e1 env) (neval e2 env)
    | NTimes (e1, e2) -> mulValues (neval e1 env) (neval e2 env)



////////////////////////////////////////////////////////////////////////
// Problem 5                                                          //
////////////////////////////////////////////////////////////////////////
// The type nexpr represents the abstract syntax of a language similar to the previous one, 
// except that pattern matching is not allowed in let bindings, but NFst and NSnd can be used to get the first 
// and second components of a pair (they correspond to fst and snd in F#).
// 5.
// (i) Write a function nexprToExpr : nexpr -> expr that converts between the two languages. 
// NFst and NSnd should be implemented by pattern matching. For every expression e : nexpr 
// and environment env : envir, the results of neval e env and eval (nexprToExpr e) env should be the same.

// (i) (Write the function nexprToExpr.)

let rec nexprToExpr (e : nexpr) : expr = 
    match e with
    | NLet(x,y,z) -> Let( PVar(x), nexprToExpr y, nexprToExpr z)
    | NPair(x,y) -> Pair(nexprToExpr x, nexprToExpr y)
    | NVar(x) -> Var(x)
    | NNum(x) -> Num(x)
    | NFst(NPair(x,y)) -> fst(nexprToExpr x, nexprToExpr y)
    | NSnd(NPair(x,y)) -> snd(nexprToExpr x, nexprToExpr y)
    // |NFst(x,y) -> fst(nexprToExpr x,nexprToExpr y)
    | NFst(x) -> nexprToExpr x
    | NSnd(y) -> nexprToExpr y
    | NPlus(x,y)  -> Plus(nexprToExpr x,nexprToExpr y)
    | NTimes(x,y) -> Times(nexprToExpr x,nexprToExpr y)


// eval (nexprToExpr (NPair (NPlus (NNum 1, NNum 2), NTimes (NNum 3, NNum 4)))) [];;
// // val it: value = VPair (VNum 3, VNum 12)
// eval (nexprToExpr (NPair (NPlus (NNum 1, NNum 2), NTimes (NNum 3, NNum 4)))) [];;
// // val it: value = VPair (VNum 3, VNum 12)
// eval (nexprToExpr (NLet ("x", NPair (NNum 4, NNum 5), NTimes (NFst (NVar "x"), NSnd (NVar "x"))))) [];;
// // val it: value = VNum 20
let v = eval (nexprToExpr (NFst (NSnd (NFst (NVar "x"))))) ["x", VPair (VPair (VPair (VNum 1, VNum 2), VPair (VNum 3, VNum 4)), VNum 5)]
// val it: value = VNum 3
let t = nexprToExpr (NFst (NPair (NNum 11, NNum 12))) 

// (ii) (Complete the function bindPattern used by exprToNexpr.)
// (ii) We can also convert from expr to nexpr, by replacing each pattern-matching 
// let binding with several ordinary let bindings that use NFst and NSnd. For example, we can convert


let rec bindPattern (p : pattern) (rhs : nexpr) (body : nexpr) : nexpr =
  match p with
  | PUnderscore -> body
  | PVar x -> NLet (x, rhs, body)
  | PPair (p1,p2) -> match p1,p2 with
                    | PPair(PVar(x), PVar(y)),PPair(PVar(m), PVar(n)) -> NPair(NPair(NVar(x),NVar(y)),NPair(NVar(m),NVar(n)))
                    | PVar(x), PVar(y) -> NVar(x)


let rec exprToNexpr (e : expr) : nexpr =
  match e with
  | Var x -> NVar x
  | Let (p, erhs, ebody) ->
      let toMatch = freshVar "toMatch" (varsInPattern p @ freeVars ebody)
      let nrhs = exprToNexpr erhs
      let nbody = exprToNexpr ebody
      let boundBody = bindPattern p (NVar toMatch) nbody
      NLet (toMatch, nrhs, boundBody)
  | Pair (e1, e2) -> NPair (exprToNexpr e1, exprToNexpr e2)
  | Num i -> NNum i
  | Plus (e1, e2) -> NPlus (exprToNexpr e1, exprToNexpr e2)
  | Times (e1, e2) -> NTimes (exprToNexpr e1, exprToNexpr e2)

// neval (exprToNexpr (Let (PPair (PVar "x", PUnderscore), Pair (Num 3, Num 4), Let (PPair (PVar "x", PUnderscore), Pair (Num 1, Num 2), Var "x")))) [];;
let r = neval (exprToNexpr (Let (PPair (PPair (PVar "x", PVar "y"), PPair (PVar "z", PVar "w")), Var "p", Plus (Var "x", Times (Var "z", Plus (Var "y", Var "w")))))) ["p", VPair (VPair (VNum 1, VNum 2), VPair (VNum 3, VNum 4))];;


type renvir = (string * int list) list
type rinstr =
    | RLoad of string
    | RStore of string
    | RErase of string
    | RNum of int
    | RAdd
    | RMul
    | RPair
    | RUnpair
    | RPop
    | RSwap
type rcode = rinstr list
type stack = int list

let rec reval (inss : rcode) (stk : stack) (renv : renvir) : int =
    let rec popValue stk =
        match stk with
        | 1 :: i :: stk -> [1; i], stk
        | tag :: stk ->
            let (vs, stk) = popValues tag stk
            tag :: vs, stk
        | _ -> failwith "reval: Too few operands on stack"
    and popValues tag stk =
        if tag < 0 then failwith "reval: Negative tag"
        else if tag = 0 then ([], stk)
        else
            let (v, stk) = popValue stk
            let (vs, stk) = popValues (tag - 1) stk
            (v @ vs, stk)

    //printfn "%A" stk
    match inss, stk with
    | [], 1 :: i :: _ -> i
    | [], _ :: i :: _ -> failwith "reval: Result is not a number!"
    | [], [] -> failwith "reval: No result on stack!"
    | RLoad x :: inss, stk -> reval inss (lookup x renv @ stk) renv
    | RStore x :: inss, stk ->
        let v, stk = popValue stk
        reval inss stk ((x, v) :: renv)
    | RErase x :: inss, stk -> reval inss stk (erase x renv)
    | RNum i :: inss, stk -> reval inss (1 :: i :: stk) renv
    | RAdd :: inss, 1 :: i2 :: 1 :: i1 :: stk -> reval inss (1 :: (i1+i2) :: stk) renv
    | RAdd :: inss, _ :: _ :: _ :: _ :: stk -> failwith "reval: expected two numbers"
    | RMul :: inss, 1 :: i2 :: 1 :: i1 :: stk -> reval inss (1 :: (i1*i2) :: stk) renv
    | RMul :: inss, _ :: _ :: _ :: _ :: stk -> failwith "reval: expected two numbers"
    | RPair :: inss, stk -> reval inss (2 :: stk) renv
    | RUnpair :: inss, 2 :: stk -> reval inss stk renv
    | RUnpair :: inss, _ :: stk -> failwith "reval: expected a pair"
    | RPop :: inss, stk ->
        let _, stk = popValue stk
        reval inss stk renv
    | RSwap :: inss, stk ->
        let v1, stk = popValue stk
        let v2, stk = popValue stk
        reval inss (v2 @ v1 @ stk) renv
    | _ -> failwith "reval: too few operands on stack"



////////////////////////////////////////////////////////////////////////
// Problem 6                                                          //
////////////////////////////////////////////////////////////////////////

(*
ANSWER 6(i) HERE:

*)

(*
ANSWER 6(ii) HERE:

*)


// (iii) (Write the functions rcompPair, rcompFst, rcompSnd used by rcomp.)

let rcompPair (r1 : rcode) (r2 : rcode) : rcode =
    failwith "Not implemented"
let rcompFst (r : rcode) : rcode =
    failwith "Not implemented"
let rcompSnd (r : rcode) : rcode =
    failwith "Not implemented"

let rec rcomp (e : nexpr) : rcode =
    match e with
    | NVar x -> [RLoad x]
    | NLet (x, erhs, ebody) -> rcomp erhs @ [RStore x] @ rcomp ebody @ [RErase x]
    | NPair (e1, e2) -> rcompPair (rcomp e1) (rcomp e2)
    | NFst e -> rcompFst (rcomp e)
    | NSnd e -> rcompSnd (rcomp e)
    | NNum i -> [RNum i]
    | NPlus (e1, e2) -> rcomp e1 @ rcomp e2 @ [RAdd]
    | NTimes (e1, e2) -> rcomp e1 @ rcomp e2 @ [RMul]




