open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
(* 
Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr
LetExpr -> let Recursion Tok_ID = Expr in Expr
Recursion -> rec | ε
FunctionExpr -> fun Tok_ID -> Expr
IfExpr -> if Expr then Expr else Expr
OrExpr -> AndExpr || OrExpr | AndExpr
AndExpr -> EqualityExpr && AndExpr | EqualityExpr
EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
EqualityOperator -> = | <>
RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
RelationalOperator -> < | > | <= | >=
AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
AdditiveOperator -> + | -
MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
MultiplicativeOperator -> * | /
ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr
UnaryExpr -> not UnaryExpr | FunctionCallExpr
FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr
PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)

let get_id_string id =
  match id with 
  | (Some (Tok_ID id_string)) -> id_string
  | _ -> raise (InvalidInputException("bad news"))
;;

let rec parse_Expr toks = 
  let next_token = lookahead toks in
  match next_token with
  (* Expr -> LetExpr *)
  | (Some Tok_Let) -> parse_LetExpr toks
  (* Expr -> IfExpr *)
  | (Some Tok_If) -> parse_IfExpr toks
  (* Expr -> FunctionExpr *)
  | (Some Tok_Fun) -> parse_FunctionExpr toks
  (* Expr -> OrExpr     EVERYTHING ELSE COMES OUT FROM HERE D: *)
  | _ -> parse_OrExpr toks

and parse_LetExpr toks =

    (* Pop off the let so that we can keep track of where we are *)
  let toks_after_let = match_token toks Tok_Let in

  match lookahead toks_after_let with
  | Some Tok_Rec ->
    let toks_after_rec = match_token toks_after_let Tok_Rec in
    let id = lookahead toks_after_rec in
    let id_string = get_id_string id in
    let toks_after_id = match_token toks_after_rec (Tok_ID id_string) in
    let toks_after_equal = match_token toks_after_id Tok_Equal in
    let (toks_after_exp1, exp1) = parse_Expr toks_after_equal in
    let toks_after_in = match_token toks_after_exp1 Tok_In in
    let (toks_after_exp2, exp2) = parse_Expr toks_after_in in
    (toks_after_exp2, Let (id_string, true, exp1, exp2))
  | Some Tok_ID id -> 
    let toks_after_id = match_token toks_after_let (Tok_ID id) in
    let toks_after_equal = match_token toks_after_id Tok_Equal in
    let (toks_after_exp1, exp1) = parse_Expr toks_after_equal in
    let toks_after_in = match_token toks_after_exp1 Tok_In in
    let (toks_after_exp2, exp2) = parse_Expr toks_after_in in
    (toks_after_exp2, Let (id, false, exp1, exp2))
  | _ -> 
    raise (InvalidInputException("uh oh"))

(* let parse_FunctionExpr =;; *)
and parse_FunctionExpr toks = 
  let toks_after_fun = match_token toks Tok_Fun in
  let id = lookahead toks_after_fun in
  let id_string = get_id_string id in
  let toks_after_id = match_token toks_after_fun (Tok_ID id_string) in
  let toks_after_arrow = match_token toks_after_id Tok_Arrow in
  let (toks_after_exp, exp) = parse_Expr toks_after_arrow in
  (toks_after_exp, Fun(id_string, exp))

(* IfExpr -> if Expr then Expr else Expr *)
and parse_IfExpr toks =
  let toks_after_if = match_token toks Tok_If in
  let (toks_after_exp1, exp1) = parse_Expr toks_after_if in
  let toks_after_then = match_token toks_after_exp1 Tok_Then in
  let (toks_after_exp2, exp2) = parse_Expr toks_after_then in
  let toks_after_else = match_token toks_after_exp2 Tok_Else in
  let (toks_after_exp3, exp3) = parse_Expr toks_after_else in
  (toks_after_exp3, If(exp1, exp2, exp3))

(* OrExpr -> AndExpr || OrExpr | AndExpr *)
(* Needs to be:
    Or_Expr -> AndExpr L
    L -> || OrExpr | epsilon *)

and parse_OrExpr toks =
  (* Pop off the let so that we can keep track of where we are *)
  let (toks_after_and, andexp) = parse_AndExpr toks in

  match lookahead toks_after_and with
  | Some Tok_Or -> 
    let toks_after_or_operator = match_token toks_after_and Tok_Or in
    let (toks_after_or, orexp) = parse_OrExpr toks_after_or_operator in
    (toks_after_or, Binop (Or, andexp, orexp))
  | _ -> 
    (toks_after_and, andexp)

and parse_AndExpr toks = 
  let (toks_after_equalityexp, equalityexp) = parse_EqualityExpr toks in

  match lookahead toks_after_equalityexp with
  | Some Tok_And -> 
    let toks_after_and_operator = match_token toks_after_equalityexp Tok_And in
    let (toks_after_and, andexp) = parse_AndExpr toks_after_and_operator in
    (toks_after_and, Binop (And, equalityexp, andexp))
  | _ -> 
    (toks_after_equalityexp, equalityexp)

and parse_EqualityExpr toks = 
  let (toks_after_relationalexp, relationalexp) = parse_RelationalExpr toks in

  match lookahead toks_after_relationalexp with
  | Some Tok_Equal -> 
    let toks_after_equality_operator = match_token toks_after_relationalexp Tok_Equal in
    let (toks_after_equalityexp, equalityexp) = parse_EqualityExpr toks_after_equality_operator in
    (toks_after_equalityexp, Binop (Equal, relationalexp, equalityexp))
  | Some Tok_NotEqual -> 
    let toks_after_equality_operator = match_token toks_after_relationalexp Tok_NotEqual in
    let (toks_after_equalityexp, equalityexp) = parse_EqualityExpr toks_after_equality_operator in
    (toks_after_equalityexp, Binop (NotEqual, relationalexp, equalityexp))
  | _ -> 
    (toks_after_relationalexp, relationalexp)

and parse_RelationalExpr toks =
  let (toks_after_additiveexp, additiveexp) = parse_AdditiveExpr toks in

  match lookahead toks_after_additiveexp with
  | Some Tok_Less -> 
    let toks_after_relational_operator = match_token toks_after_additiveexp Tok_Less in
    let (toks_after_relationalexp, relationalexp) = parse_RelationalExpr toks_after_relational_operator in
    (toks_after_relationalexp, Binop (Less, additiveexp, relationalexp))
  | Some Tok_LessEqual -> 
    let toks_after_relational_operator = match_token toks_after_additiveexp Tok_LessEqual in
    let (toks_after_relationalexp, relationalexp) = parse_RelationalExpr toks_after_relational_operator in
    (toks_after_relationalexp, Binop (LessEqual, additiveexp, relationalexp))
  | Some Tok_Greater -> 
    let toks_after_relational_operator = match_token toks_after_additiveexp Tok_Greater in
    let (toks_after_relationalexp, relationalexp) = parse_RelationalExpr toks_after_relational_operator in
    (toks_after_relationalexp, Binop (Greater, additiveexp, relationalexp))
  | Some Tok_GreaterEqual -> 
    let toks_after_relational_operator = match_token toks_after_additiveexp Tok_GreaterEqual in
    let (toks_after_relationalexp, relationalexp) = parse_RelationalExpr toks_after_relational_operator in
    (toks_after_relationalexp, Binop (GreaterEqual, additiveexp, relationalexp))
  | _ -> 
    (toks_after_additiveexp, additiveexp)

and parse_AdditiveExpr toks = 
  let (toks_after_multiplicativeexp, multiplicativeexp) = parse_MultiplicativeExpr toks in

  match lookahead toks_after_multiplicativeexp with
  | Some Tok_Add -> 
    let toks_after_additive_operator = match_token toks_after_multiplicativeexp Tok_Add in
    let (toks_after_additiveexp, additiveexp) = parse_AdditiveExpr toks_after_additive_operator in
    (toks_after_additiveexp, Binop (Add, multiplicativeexp, additiveexp))
  | Some Tok_Sub -> 
    let toks_after_additive_operator = match_token toks_after_multiplicativeexp Tok_Sub in
    let (toks_after_additiveexp, additiveexp) = parse_AdditiveExpr toks_after_additive_operator in
    (toks_after_additiveexp, Binop (Sub, multiplicativeexp, additiveexp))
  | _ -> 
    (toks_after_multiplicativeexp, multiplicativeexp)

and parse_MultiplicativeExpr toks = 
  let (toks_after_concatexp, concatexp) = parse_ConcatExpr toks in

  match lookahead toks_after_concatexp with
  | Some Tok_Mult -> 
    let toks_after_multiplicative_operator = match_token toks_after_concatexp Tok_Mult in
    let (toks_after_multiplicativeexp, multiplicativeexp) = parse_MultiplicativeExpr toks_after_multiplicative_operator in
    (toks_after_multiplicativeexp, Binop (Mult, concatexp, multiplicativeexp))
  | Some Tok_Div -> 
    let toks_after_multiplicative_operator = match_token toks_after_concatexp Tok_Div in
    let (toks_after_multiplicativeexp, multiplicativeexp) = parse_MultiplicativeExpr toks_after_multiplicative_operator in
    (toks_after_multiplicativeexp, Binop (Div, concatexp, multiplicativeexp))
  | _ -> 
    (toks_after_concatexp, concatexp)

and parse_ConcatExpr toks = 
  let (toks_after_unaryexp, unaryexp) = parse_UnaryExpr toks in

  match lookahead toks_after_unaryexp with
  | Some Tok_Concat -> 
    let toks_after_concat = match_token toks_after_unaryexp Tok_Concat in
    let (toks_after_concatexp, concatexp) = parse_ConcatExpr toks_after_concat in
    (toks_after_concatexp, Binop (Concat, unaryexp, concatexp))
  | _ -> 
    (toks_after_unaryexp, unaryexp)

and parse_UnaryExpr toks = 
  match lookahead toks with
  | Some Tok_Not ->
    let toks_after_not = match_token toks Tok_Not in
    let (toks_after_unaryexp, unaryexp) = parse_UnaryExpr toks_after_not in
    (toks_after_unaryexp, Not(unaryexp))
  | _ ->
    parse_FunctionCallExpr toks
    

and parse_FunctionCallExpr toks = 
  let (toks_after_exp1, exp1) = parse_PrimaryExpr toks in

  match lookahead toks_after_exp1 with
  | Some Tok_Int int -> 
    let (toks_after_exp2, exp2) = parse_PrimaryExpr toks_after_exp1 in
    (toks_after_exp2, FunctionCall (exp1, exp2))
  | Some Tok_Bool bool -> 
    let (toks_after_exp2, exp2) = parse_PrimaryExpr toks_after_exp1 in
    (toks_after_exp2, FunctionCall (exp1, exp2))
  | Some Tok_String string -> 
    let (toks_after_exp2, exp2) = parse_PrimaryExpr toks_after_exp1 in
    (toks_after_exp2, FunctionCall (exp1, exp2))
  | Some Tok_ID id -> 
    let (toks_after_exp2, exp2) = parse_PrimaryExpr toks_after_exp1 in
    (toks_after_exp2, FunctionCall (exp1, exp2))
  | Some Tok_LParen -> 
    let (toks_after_exp2, exp2) = parse_PrimaryExpr toks_after_exp1 in
    (toks_after_exp2, FunctionCall (exp1, exp2))
  | _ -> 
    (toks_after_exp1, exp1)



and parse_PrimaryExpr toks = 
  let next_token = lookahead toks in
    match next_token with
    (* PrimaryExpr -> Tok_Int *)
    | Some Tok_Int int -> (match_token toks (Tok_Int int), Value(Int int))
    (* PrimaryExpr -> Tok_Bool *)
    | Some Tok_Bool bool -> (match_token toks (Tok_Bool bool), Value(Bool bool))
    (* PrimaryExpr -> Tok_String *)
    | Some Tok_String string -> (match_token toks (Tok_String string), Value(String string))
    (* PrimaryExpr -> Tok_ID *)
    | Some Tok_ID id -> (match_token toks (Tok_ID id), (ID id))
    (* PrimaryExpr -> (Expr) *)
    | Some Tok_LParen -> 
      let toks_after_lparen = match_token toks Tok_LParen in
      let (toks_after_exp, exp) = parse_Expr toks_after_lparen in
      let toks_after_rparen = match_token toks_after_exp Tok_RParen in
      (toks_after_rparen, exp)
    | _ -> raise (InvalidInputException(
        Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_list string_of_token toks)
        (string_of_list string_of_token toks)
        (string_of_list string_of_token toks)))
    
;;


let rec parse_expr toks =

  parse_Expr toks

;;



(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 

  let next_token = lookahead toks in
  match next_token with
  (* Mutop -> DefMutop *)
  | (Some Tok_Def) -> 
    parse_DefMutop toks
  (* Mutop -> ;; *)
  | (Some Tok_DoubleSemi) -> 
    let toks_after_double_semi = match_token toks Tok_DoubleSemi in
    (toks_after_double_semi, NoOp)
  (* Mutop -> ExprMutop *)
  | _ -> 
    parse_ExprMutop toks

and parse_DefMutop toks = 
  let toks_after_def = match_token toks Tok_Def in
  let id = lookahead toks_after_def in
  let id_string = get_id_string id in
  let toks_after_id = match_token toks_after_def (Tok_ID id_string) in
  let toks_after_equal = match_token toks_after_id Tok_Equal in
  let (toks_after_exp, exp) = parse_Expr toks_after_equal in
  let toks_after_double_semi = match_token toks_after_exp Tok_DoubleSemi in
  (toks_after_double_semi, (Def (id_string, exp)))

and parse_ExprMutop toks = 
  let (toks_after_exp, exp) = parse_Expr toks in
  let toks_after_double_semi = match_token toks_after_exp Tok_DoubleSemi in
  (toks_after_double_semi, Expr exp)