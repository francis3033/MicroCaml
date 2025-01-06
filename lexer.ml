open Types
exception InvalidInputException of string
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_num = Str.regexp "\\([0-9]+\\)"
let re_negnum = Str.regexp "(-[0-9]+)"
let re_arrow = Str.regexp "->"
let re_rparen = Str.regexp ")"
let re_lparen = Str.regexp "("
let re_lcurly = Str.regexp "{"
let re_rcurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_doublesemi = Str.regexp ";;"
let re_bool = Str.regexp "true\\|false"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_semi = Str.regexp ";"
let re_whitespace = Str.regexp "[\n\t ]+"


let tokenize input =
  let rec process curr tokens =
    if curr >= String.length input then tokens
    else
      let matchedToken, nextIndex =
        if Str.string_match re_negnum input curr then
          [Tok_Int (int_of_string (Str.matched_group 1 input))], curr + String.length (Str.matched_string input)
        else if Str.string_match re_num input curr then
          [Tok_Int (int_of_string (Str.matched_string input))], curr + String.length (Str.matched_string input)
        else if Str.string_match re_arrow input curr then
          [Tok_Arrow], curr + 2
        else if Str.string_match re_rparen input curr then
          [Tok_RParen], curr + 1
        else if Str.string_match re_lparen input curr then
          [Tok_LParen], curr + 1
        else if Str.string_match re_lcurly input curr then
          [Tok_LCurly], curr + 1
        else if Str.string_match re_rcurly input curr then
          [Tok_RCurly], curr + 1
        else if Str.string_match re_dot input curr then
          [Tok_Dot], curr + 1
        else if Str.string_match re_equal input curr then
          [Tok_Equal], curr + 1
        else if Str.string_match re_notequal input curr then
          [Tok_NotEqual], curr + 2
        else if Str.string_match re_greater input curr then
          [Tok_Greater], curr + 1
        else if Str.string_match re_less input curr then
          [Tok_Less], curr + 1
        else if Str.string_match re_greaterequal input curr then
          [Tok_GreaterEqual], curr + 2
        else if Str.string_match re_lessequal input curr then
          [Tok_LessEqual], curr + 2
        else if Str.string_match re_or input curr then
          [Tok_Or], curr + 2
        else if Str.string_match re_and input curr then
          [Tok_And], curr + 2
        else if Str.string_match re_not input curr then
          [Tok_Not], curr + 3
        else if Str.string_match re_if input curr then
          [Tok_If], curr + 2
        else if Str.string_match re_then input curr then
          [Tok_Then], curr + 4
        else if Str.string_match re_else input curr then
          [Tok_Else], curr + 4
        else if Str.string_match re_add input curr then
          [Tok_Add], curr + 1
        else if Str.string_match re_sub input curr then
          [Tok_Sub], curr + 1
        else if Str.string_match re_mult input curr then
          [Tok_Mult], curr + 1
        else if Str.string_match re_div input curr then
          [Tok_Div], curr + 1
        else if Str.string_match re_concat input curr then
          [Tok_Concat], curr + 1
        else if Str.string_match re_let input curr then
          [Tok_Let], curr + 3
        else if Str.string_match re_rec input curr then
          [Tok_Rec], curr + 3
        else if Str.string_match re_in input curr then
          [Tok_In], curr + 2
        else if Str.string_match re_def input curr then
          [Tok_Def], curr + 3
        else if Str.string_match re_fun input curr then
          [Tok_Fun], curr + 3
        else if Str.string_match re_doublesemi input curr then
          [Tok_DoubleSemi], curr + 2
        else if Str.string_match re_bool input curr then
          let booleanValue = Str.matched_string input in
          [Tok_Bool (bool_of_string booleanValue)], curr + String.length booleanValue
        else if Str.string_match re_string input curr then
          let stringValue = Str.matched_string input in
          let cleanString = Str.global_replace (Str.regexp "\"") "" stringValue in
          if String.length cleanString = 0 then raise (InvalidInputException "Invalid string")
          else [Tok_String cleanString], curr + String.length stringValue
        else if Str.string_match re_id input curr then
          let identifier = Str.matched_string input in
          [Tok_ID identifier], curr + String.length identifier
        else if Str.string_match re_semi input curr then
          [Tok_Semi], curr + 1
        else if Str.string_match re_whitespace input curr then
          [], curr + String.length (Str.matched_string input)
        else
          raise (InvalidInputException "Invalid input"), curr + 1
      in
      process nextIndex (tokens @ matchedToken)
  in
  process 0 []
