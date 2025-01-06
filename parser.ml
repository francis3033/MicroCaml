open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of toks given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

(* Helper function to process recursion in let bindings *)
let process_rec toks =
  match lookahead toks with
  | Some Tok_Rec -> (match_token toks Tok_Rec, true)
  | _ -> (toks, false)

(* Helper function to ensure an identifier follows and match it *)
let expect_id toks =
  match lookahead toks with
  | Some (Tok_ID x) -> (match_token toks (Tok_ID x), x)
  | _ -> raise (InvalidInputException "Error in parse_let")

 
let rec parse_expr all_toks =
    let (toks_remain, parsed_expr) = parse_let all_toks in
    (toks_remain, parsed_expr)

and parse_let input_toks =
  match lookahead input_toks with
  | Some Tok_Let ->
    let toks_after_let = match_token input_toks Tok_Let in
    let (toks_after_rec, is_rec) = process_rec toks_after_let in
    let (toks_before_eq, id) = expect_id toks_after_rec in
    let toks_after_equal = match_token toks_before_eq Tok_Equal in
    let (toks_before_in, expr1) = parse_expr toks_after_equal in
    let toks_after_in = match_token toks_before_in Tok_In in
    let (final_toks, expr2) = parse_expr toks_after_in in
    (final_toks, Let (id, is_rec, expr1, expr2))
  | _ -> parse_fun input_toks

and parse_fun toks =
  match lookahead toks with
  | Some Tok_Fun ->
    let toks_after_fun = match_token toks Tok_Fun in
    let (toks_before_arrow, func_var) = expect_id toks_after_fun in
    let toks_after_arrow = match_token toks_before_arrow Tok_Arrow in
    let (final_toks, body) = parse_expr toks_after_arrow in
    (final_toks, Fun (func_var, body))
  | _ -> parse_if toks

and parse_if toks =
  match lookahead toks with
  | Some Tok_If ->
    let toks_after_if = match_token toks Tok_If in
    let (toks_after_cond, cond_expr) = parse_expr toks_after_if in
    let toks_after_then = match_token toks_after_cond Tok_Then in
    let (toks_after_then_expr, then_expr) = parse_expr toks_after_then in
    let toks_after_else = match_token toks_after_then_expr Tok_Else in
    let (final_toks, else_expr) = parse_expr toks_after_else in
    (final_toks, If (cond_expr, then_expr, else_expr))
  | _ -> parse_or toks

and parse_or toks =
  let (toks_after_l, lhs) = parse_and toks in
  let rec handle_or_ops toks_acc expr_acc =
    match lookahead toks_acc with
    | Some Tok_Or ->
      let toks_next = match_token toks_acc Tok_Or in
      let (toks_final, expr_next) = parse_or toks_next in
      handle_or_ops toks_final (Binop (Or, expr_acc, expr_next))
    | _ -> (toks_acc, expr_acc)
  in
  handle_or_ops toks_after_l lhs
  
and parse_eq toks =
  let (toks_after_l, l_expr) = parse_relation toks in
  match lookahead toks_after_l with
  | Some token when token = Tok_Equal || token = Tok_NotEqual ->
    let toks_after_op = match_token toks_after_l token in
    let (final_toks, r_expr) = parse_eq toks_after_op in
    let bin_op = if token = Tok_Equal then Equal else NotEqual in
    (final_toks, Binop (bin_op, l_expr, r_expr))
  | _ -> (toks_after_l, l_expr)
      
and parse_and toks =
  let rec process_and_acc toks acc =
    match lookahead toks with
    | Some Tok_And ->
      let toks_next = match_token toks Tok_And in
      let (toks_after, next_expr) = parse_eq toks_next in
      process_and_acc toks_after (Binop (And, acc, next_expr))
    | _ -> (toks, acc)
  in
  let (toks_after_l, l_expr) = parse_eq toks in
  process_and_acc toks_after_l l_expr
   
and parse_relation toks =
  let (toks_after_l, l_expr) = parse_add toks in
  match lookahead toks_after_l with
  | Some (Tok_Less | Tok_Greater | Tok_LessEqual | Tok_GreaterEqual as op) ->
    let toks_after_op = match_token toks_after_l op in
    let (final_toks, r_expr) = parse_relation toks_after_op in
    let bin_op = match op with
      | Tok_Less -> Less
      | Tok_Greater -> Greater
      | Tok_LessEqual -> LessEqual
      | Tok_GreaterEqual -> GreaterEqual
      | _ -> raise (InvalidInputException "Unexpected relational operator")
    in
    (final_toks, Binop (bin_op, l_expr, r_expr))
  | _ -> (toks_after_l, l_expr)
  
and parse_mul toks =
  let rec parse_mul_chain toks acc_expr =
    match lookahead toks with
    | Some op when op = Tok_Mult || op = Tok_Div ->
      let toks_next = match_token toks op in
      let (toks_after_op, next_expr) = parse_concat toks_next in
      let new_expr = match op with
        | Tok_Mult -> Binop (Mult, acc_expr, next_expr)
        | Tok_Div -> Binop (Div, acc_expr, next_expr)
        | _ -> acc_expr
      in
      parse_mul_chain toks_after_op new_expr
    | _ -> (toks, acc_expr)
  in
  let (toks_after_first, first_expr) = parse_concat toks in
  parse_mul_chain toks_after_first first_expr

and parse_add toks =
  let (toks_after_l, l_expr) = parse_mul toks in
  match lookahead toks_after_l with
  | Some Tok_Add ->
    let toks_after_add = match_token toks_after_l Tok_Add in
    let (final_toks, r_expr) = parse_add toks_after_add in
    (final_toks, Binop (Add, l_expr, r_expr))
  | Some Tok_Sub ->
    let toks_after_sub = match_token toks_after_l Tok_Sub in
    let (final_toks, r_expr) = parse_add toks_after_sub in
    (final_toks, Binop (Sub, l_expr, r_expr))
  | _ -> (toks_after_l, l_expr)
  
and parse_concat toks =
  let rec parse_concat_chain toks acc_expr =
    match lookahead toks with
    | Some Tok_Concat ->
      let toks_next = match_token toks Tok_Concat in
      let (toks_after_op, next_expr) = parse_unary toks_next in
      parse_concat_chain toks_after_op (Binop (Concat, acc_expr, next_expr))
    | _ -> (toks, acc_expr)
  in
  let (toks_after_l, l_expr) = parse_unary toks in
  parse_concat_chain toks_after_l l_expr

and parse_unary toks =
  let rec handle_negations toks acc =
    match lookahead toks with
    | Some Tok_Not ->
      let toks_next = match_token toks Tok_Not in
      handle_negations toks_next (acc + 1)
    | _ ->
      let (final_toks, expr) = parse_app toks in
      if acc mod 2 == 1 then (final_toks, Not expr) else (final_toks, expr)
  in
  handle_negations toks 0
  
and parse_select toks =
  let (toks_after_l, select_expr) = parse_primary toks in
  match lookahead toks_after_l with
  | Some Tok_Dot ->
    let toks_after_dot = match_token toks_after_l Tok_Dot in
    begin match lookahead toks_after_dot with
    | Some (Tok_ID id) ->
      let toks_after_id = match_token toks_after_dot (Tok_ID id) in
      (toks_after_id, Select (Lab id, select_expr))
    | _ -> raise (InvalidInputException "parse_select") end
  | _ -> (toks_after_l, select_expr)
  
and parse_app toks =
  let (toks_after_l, l_expr) = parse_select toks in
  match lookahead toks_after_l with
  | Some (Tok_Int i) -> 
    let toks_after = match_token toks_after_l (Tok_Int i) in
    (toks_after, App (l_expr, Int i))
  | Some (Tok_Bool b) ->
    let toks_after = match_token toks_after_l (Tok_Bool b) in
    (toks_after, App (l_expr, Bool b))
  | Some (Tok_String s) ->
    let toks_after = match_token toks_after_l (Tok_String s) in
    (toks_after, App (l_expr, String s))
  | Some (Tok_ID id) ->
    let toks_after = match_token toks_after_l (Tok_ID id) in
    (toks_after, App (l_expr, ID id))
  | Some Tok_LParen ->
    let toks_after_lparen = match_token toks_after_l Tok_LParen in
    let (toks_before_rparen, expr) = parse_expr toks_after_lparen in
    let toks_after_rparen = match_token toks_before_rparen Tok_RParen in
    (toks_after_rparen, App (l_expr, expr))
  | Some Tok_LCurly ->
    parse_record toks_after_l
  | _ -> (toks_after_l, l_expr)

and parse_primary toks =
  let process_simple_tok tok expr_constructor =
    let toks_after = match_token toks tok in
    (toks_after, expr_constructor)
  in
  match lookahead toks with
  | Some (Tok_Int i) -> process_simple_tok (Tok_Int i) (Int i)
  | Some (Tok_Bool b) -> process_simple_tok (Tok_Bool b) (Bool b)
  | Some (Tok_String s) -> process_simple_tok (Tok_String s) (String s)
  | Some (Tok_ID id) -> process_simple_tok (Tok_ID id) (ID id)
  | Some Tok_LParen ->
    let toks_after_lparen = match_token toks Tok_LParen in
    let (final_toks, expr) = parse_expr toks_after_lparen in
    let toks_after_rparen = match_token final_toks Tok_RParen in
    (toks_after_rparen, expr)
  | Some Tok_LCurly -> parse_record toks
  | _ -> raise (InvalidInputException "Expected a valid primary expression")

and parse_record toks =
  let rec parse_record_contents toks acc =
    match lookahead toks with
    | Some Tok_RCurly ->
      let toks_next = match_token toks Tok_RCurly in
      (toks_next, Record acc)
    | _ ->
      let (toks_after_field, new_fields) = parse_record_body toks in
      parse_record_contents toks_after_field (acc @ new_fields)
  in
  let toks_after_lcurly = match_token toks Tok_LCurly in
  parse_record_contents toks_after_lcurly []

and parse_record_body toks =
  let rec collect_fields toks fields_acc =
    match lookahead toks with
    | Some (Tok_ID id) ->
      let toks_after_id = match_token toks (Tok_ID id) in
      let toks_after_equal = match_token toks_after_id Tok_Equal in
      let (toks_after_expr, expr) = parse_expr toks_after_equal in
      let new_fields = (Lab id, expr) :: fields_acc in
      begin match lookahead toks_after_expr with
      | Some Tok_Semi ->
        let toks_after_semi = match_token toks_after_expr Tok_Semi in
        collect_fields toks_after_semi new_fields
      | _ -> (toks_after_expr, List.rev new_fields)
      end
    | _ -> raise (InvalidInputException "parse_record_body error")
  in
  collect_fields toks []
  
(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def -> parse_def toks
  | Some Tok_DoubleSemi -> parse_no_op toks
  | _ -> parse_expr_semi toks

and parse_def toks =
  let toks_after_def = match_token toks Tok_Def in
  match lookahead toks_after_def with
  | Some (Tok_ID id) ->
    let toks_after_id = match_token toks_after_def (Tok_ID id) in
    let toks_after_eq = match_token toks_after_id Tok_Equal in
    let (toks_after_expr, expr) = parse_expr toks_after_eq in
    ensure_double_semi toks_after_expr (Def(id, expr))
  | _ -> raise (InvalidInputException "Invalid input")

and ensure_double_semi toks parsed_data =
  let final_toks = match_token toks Tok_DoubleSemi in
  (final_toks, parsed_data)

and parse_no_op toks =
  let toks_after_dsemi = match_token toks Tok_DoubleSemi in
  (toks_after_dsemi, NoOp)

and parse_expr_semi toks =
  let (toks_before_dsemi, expr) = parse_expr toks in
  ensure_double_semi toks_before_dsemi (Expr(expr))
