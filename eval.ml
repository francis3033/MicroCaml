open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to env [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

let rec eval_expr env expr = match expr with
  | Int num -> Int num
  | Bool bool_val -> Bool bool_val
  | String str -> String str
  | ID identifier ->
    (try match !(List.assoc identifier env) with
    | value -> value
    | exception Not_found -> raise (DeclareError "Invalid variable") with Not_found -> raise (DeclareError "Invalid variable"))
  | Fun (arg, body) -> Closure (env, arg, body)
  | Not inner_expr ->
    begin match eval_expr env inner_expr with
    | Bool result -> Bool (not result)
    | _ -> raise (TypeError "Expected type bool") end
  | Binop (operator, left_expr, right_expr) ->
    let left_val = eval_expr env left_expr in
    let right_val = eval_expr env right_expr in begin match operator, left_val, right_val with
    | Add, Int left_int, Int right_int -> Int (left_int + right_int)
    | Sub, Int left_int, Int right_int -> Int (left_int - right_int)
    | Mult, Int left_int, Int right_int -> Int (left_int * right_int)
    | Div, Int left_int, Int right_int ->
      if right_int = 0 then raise (DivByZeroError)
      else Int (left_int / right_int)
    | Concat, String left_str, String right_str -> String (left_str ^ right_str)
    | Equal, Int left_int, Int right_int -> Bool (left_int = right_int)
    | NotEqual, Int left_int, Int right_int -> Bool (left_int <> right_int)
    | Greater, Int left_int, Int right_int -> Bool (left_int > right_int)
    | Less, Int left_int, Int right_int -> Bool (left_int < right_int)
    | GreaterEqual, Int left_int, Int right_int -> Bool (left_int >= right_int)
    | LessEqual, Int left_int, Int right_int -> Bool (left_int <= right_int)
    | Or, Bool left_bool, Bool right_bool -> Bool (left_bool || right_bool)
    | And, Bool left_bool, Bool right_bool -> Bool (left_bool && right_bool)
    | _, _, _ -> raise (TypeError "Invalid type for binary operation") end
  | If (condition, true_expr, false_expr) ->
    begin match eval_expr env condition with
    | Bool true -> eval_expr env true_expr
    | Bool false -> eval_expr env false_expr
    | _ -> raise (TypeError "Error with if expression") end
  | App (func_expr, arg_expr) ->
    let closure = eval_expr env func_expr in
    let arg_val = eval_expr env arg_expr in begin match closure with
    | Closure (closure_env, arg, body) ->
    let extended_env = extend closure_env arg arg_val in
    eval_expr extended_env body
    | _ -> raise (TypeError "Invalid input") end
  | Let (variable, is_recursive, initial_expr, body_expr) ->
    if is_recursive then
      let placeholder_env = extend_tmp env variable in
      let init_val = eval_expr placeholder_env initial_expr in
      update placeholder_env variable init_val;  
      eval_expr placeholder_env body_expr  
    else
      let init_val = eval_expr env initial_expr in
      let new_env = extend env variable init_val in
      eval_expr new_env body_expr
  | Record fields ->
    let evaluated_fields = List.fold_left (fun acc (label, expr) -> 
      (label, eval_expr env expr) :: acc) [] fields in
    Record (List.rev evaluated_fields)    
  | Select (label, record_expr) ->
    begin match eval_expr env record_expr with
    | Record field_list ->
      (try List.assoc label field_list with Not_found -> raise (SelectError "Invalid Label"))
    | _ -> raise (TypeError "Invalid Record")
    end
  | _ -> raise (InvalidInputException "Invalid expression")


(* Part 2: Evaluating mutop directive *)


let eval_mutop env m =
  let handle_def environment variable expr =
    let temp_env = extend_tmp environment variable in
    let evaluated_expr = eval_expr temp_env expr in
    update temp_env variable evaluated_expr;
    (temp_env, Some evaluated_expr)
  in
  let handle_expr environment expr =
    let evaluated_expr = eval_expr environment expr in
    (environment, Some evaluated_expr)
  in
  match m with
  | Def (var, expr) -> handle_def env var expr
  | Expr e1 -> handle_expr env e1
  | NoOp -> (env, None)