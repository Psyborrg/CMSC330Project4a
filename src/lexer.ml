open TokenTypes
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let re_num = Str.regexp "[0-9]+" (* single digit *)
let re_neg = Str.regexp "(-[0-9]+)" 
let re_bool_true = Str.regexp "^true$"
let re_bool_false = Str.regexp "^false$"
let re_rec = Str.regexp "^rec$"
let re_let = Str.regexp "^let$"
let re_in = Str.regexp "^in$"
let re_def = Str.regexp "^def$"
let re_fun = Str.regexp "^fun$"
let re_not = Str.regexp "^not$"
let re_if = Str.regexp "^if$"
let re_then = Str.regexp "^then$"
let re_else = Str.regexp "^else$"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_concat = Str.regexp "\\^"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_whitespace = Str.regexp "[ \t\n]+"


let tokenize input = 

    let rec tok str pos =
	    if pos < String.length str then

            (* Handle negative integers "(-123)" *)
            if Str.string_match re_neg str pos then
                let matched = Str.matched_string str in
                let matched_length = String.length matched in
		        let head = Tok_Int (int_of_string (String.sub matched 1 ((matched_length) - 2))) in
		        head :: tok str (pos + (String.length matched))
            (* Handle positive integers "123" *)
            else if Str.string_match re_num str pos then
                let matched = Str.matched_string str in
		        let head = Tok_Int (int_of_string (matched)) in
		        head :: tok str (pos + (String.length matched))

            (* Handle "arrow" *)
            else if Str.string_match re_arrow str pos then
                let matched = Str.matched_string str in
		        let head = Tok_Arrow in
		        head :: tok str (pos + (String.length matched))
            (* Handle ";;" *)
            else if Str.string_match re_doublesemi str pos then
                let matched = Str.matched_string str in
		        let head = Tok_DoubleSemi in
		        head :: tok str (pos + (String.length matched))

            (* Handle strings *)
            else if Str.string_match re_string str pos then
                let matched = Str.matched_string str in
                let cleaned_string = String.sub matched (1) (String.length matched - 2) in
		        let head = Tok_String cleaned_string in
		        head :: tok str (pos + (String.length matched))

            (* Handle ID's *)
            else if Str.string_match re_id str pos then
                let initial_match = Str.matched_string str in

                (* Now check for each special case of ID, that being the tokens that look like ID's but are not *)
                 
                (* Handle booleans *)
                if Str.string_match re_bool_true initial_match 0 then
                    let head = Tok_Bool (true) in
                    head :: tok str (pos + String.length initial_match)
                else if Str.string_match re_bool_false initial_match 0 then
                    let head = Tok_Bool (false) in
                    head :: tok str (pos + String.length initial_match)

                (* Handle "not" *)
                else if Str.string_match re_not initial_match 0 then
                    let head = Tok_Not in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "if" *)
                else if Str.string_match re_if initial_match 0 then
                    let head = Tok_If in
                    head :: tok str (pos + (String.length initial_match))

                (* Handle "then" *)
                else if Str.string_match re_then initial_match 0 then
                    let head = Tok_Then in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle rec *)
                else if Str.string_match re_rec initial_match 0 then
                    let head = Tok_Rec in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "else" *)
                else if Str.string_match re_else initial_match 0 then
                    let head = Tok_Else in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "let" *)
                else if Str.string_match re_let initial_match 0 then
                    let head = Tok_Let in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "in" *)
                else if Str.string_match re_in initial_match 0 then
                    let head = Tok_In in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "def" *)
                else if Str.string_match re_def initial_match 0 then
                    let head = Tok_Def in
                    head :: tok str (pos + (String.length initial_match))
                (* Handle "fun" *)
                else if Str.string_match re_fun initial_match 0 then
                    let head = Tok_Fun in
                    head :: tok str (pos + (String.length initial_match))
                else
                    let head = Tok_ID initial_match in
                    head :: tok str (pos + String.length initial_match)

                (* HANDLE MATH OPERATIONS *)
                (* Handle "+" *)
                else if Str.string_match re_add str pos then
                    let head = Tok_Add in
                    head :: tok str (pos + 1)
                    (* Handle "-" *)
                else if Str.string_match re_sub str pos then
                    let head = Tok_Sub in
                    head :: tok str (pos + 1)
                    (* Handle "*" *)
                else if Str.string_match re_mult str pos then
                    let head = Tok_Mult in
                    head :: tok str (pos + 1)
                    (* Handle "/" *)
                else if Str.string_match re_div str pos then
                    let head = Tok_Div in
                    head :: tok str (pos + 1)

                (* HANDLE INEQUALITIES *)
                (* Handle "=" *)
                else if Str.string_match re_equal str pos then
                    let head = Tok_Equal in
                    head :: tok str (pos + 1)
                (* Handle not equal *)
                else if Str.string_match re_notequal str pos then
                    let head = Tok_NotEqual in
                    head :: tok str (pos + 2)
                (* Handle less than *)
                else if Str.string_match re_less str pos then
                    let head = Tok_Less in
                    head :: tok str (pos + 1)
                (* Handle less than equal to *)
                else if Str.string_match re_lessequal str pos then
                    let head = Tok_LessEqual in
                    head :: tok str (pos + 2)
                (* Handle greater than *)
                else if Str.string_match re_greater str pos then
                    let head = Tok_Greater in
                    head :: tok str (pos + 1)
                (* Handle greater than equal to *)
                else if Str.string_match re_greaterequal str pos then
                    let head = Tok_GreaterEqual in
                    head :: tok str (pos + 2)

                (* Handle "(" *)
                else if Str.string_match re_lparen str pos then
                    let head = Tok_LParen in
                    head :: tok str (pos + 1)
                (* Handle ")" *)
                else if Str.string_match re_rparen str pos then
                    let head = Tok_RParen in
                    head :: tok str (pos + 1)

            (* Handle logical operators *)
            (* Handle "||" *)
            else if Str.string_match re_or str pos then
		        let head = Tok_Or in
		        head :: tok str (pos + 2)

            (* Handle "&&" *)
            else if Str.string_match re_and str pos then
		        let head = Tok_And in
		        head :: tok str (pos + 2)
            
            (* Handle "concatonation" *)
            else if Str.string_match re_concat str pos then
		        let head = Tok_Concat in
		        head :: tok str (pos + 1)

            (* Handle whitespace, including spaces tabs and newlines *)
            else if Str.string_match re_whitespace str pos then
                let matched = Str.matched_string str in
                tok str (pos + (String.length matched))
            else
                []
	    else
		    []
    in tok input 0 
;;
