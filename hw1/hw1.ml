(* hw1.ml
 * Handling infix expressions with percents:
 *
 *   x + y %
 *   x - y %
 *   x * y %
 *
 * Programmer: Mayer Goldberg, 2024
 *)

 #use "pc.ml";;

 (* Type definitions moved before the module signature *)
 
 type binop = Add | Sub | Mul | Div | Mod | Pow | AddPer | SubPer | PerOf;;
 
 type expr =
   | Num of int
   | Var of string
   | BinOp of binop * expr * expr
   | Deref of expr * expr
   | Call of expr * expr list
   | Percent of expr;;
 
 type args_or_index = Args of expr list | Index of expr;;
 
 module type INFIX_PARSER = sig
   val nt_expr : expr PC.parser
 end;; (* module type INFIX_PARSER *)
 
 module InfixParser : INFIX_PARSER = struct
 open PC
 
 
 
 
 
 
 
 (* Helper functions *)
 
 (* Helper function to convert a list of characters to a string *)
 let list_to_string chars =
   String.concat "" (List.map (String.make 1) chars)
 
 (* Parser for whitespace *)
 let nt_whitespace =
   star (const (fun ch -> ch <= ' '))
 
 (* Function to make a parser skip whitespace around it *)
 let make_padded nt =
   let nt = caten nt_whitespace (caten nt nt_whitespace) in
   pack nt (fun (_, (v, _)) -> v)
 
 (* Parser for numbers *)
 let nt_num_no_ws =
   let nt_sign = maybe (char '-') in
   let nt_digits = plus (range '0' '9') in
   pack (caten nt_sign nt_digits)
     (fun (sign, digits) ->
        let n = int_of_string (list_to_string digits) in
        match sign with
        | Some _ -> Num (-n)
        | None -> Num n)
 
 let nt_num = make_padded nt_num_no_ws


 let nt_reserved_keywords =
  make_padded (pack (word "mod") (fun _ -> "mod"))


 let nt_var_no_ws =
    let nt_head = range_ci 'a' 'z' in
    let nt_tail = star (disj_list [range 'a' 'z'; range 'A' 'Z'; range '0' '9'; char '_'; char '$']) in
    let nt_var = pack (caten nt_head nt_tail)
                       (fun (head, tail) -> list_to_string (head :: tail)) in
    pack nt_var (fun var ->
      match var with
      | "mod" -> raise PC.X_no_match  
      | _ -> Var var  
    )
  

 let nt_var = make_padded nt_var_no_ws

 
 (* Operator parsers *)
 let nt_add = make_padded (pack (char '+') (fun _ -> Add))
 let nt_sub = make_padded (pack (char '-') (fun _ -> Sub))
 let nt_mul = make_padded (pack (char '*') (fun _ -> Mul))
 let nt_div = make_padded (pack (char '/') (fun _ -> Div))
 let nt_pow = make_padded (pack (char '^') (fun _ -> Pow))
 let nt_mod = make_padded (pack (word "mod") (fun _ -> Mod))
 let nt_percent = make_padded (char '%')
 
 (* Parentheses and brackets parsers *)
 let nt_lparen = make_padded (char '(')
 let nt_rparen = make_padded (char ')')
 let nt_comma = make_padded (char ',')
 let nt_lbracket = make_padded (char '[')
 let nt_rbracket = make_padded (char ']')
 
 
 let rec nt_expr s pos =
   nt_add_sub_expr s pos
 

 and nt_add_sub_expr s pos =
   let nt = caten nt_mul_div_expr
     (star (caten (disj nt_add nt_sub) nt_mul_div_expr)) in
   pack nt (fun (e1, rest) ->
     List.fold_left (fun acc (op, e) ->
       match (op, e) with
       | (Add, Percent e2) -> BinOp (AddPer, acc, e2)
       | (Sub, Percent e2) -> BinOp (SubPer, acc, e2)
       | (op, e) -> BinOp (op, acc, e)
     ) e1 rest) s pos
     

 
  and nt_mul_div_expr s pos =
    let nt = caten nt_pow_expr
      (star (caten (disj_list [nt_mul; nt_div; nt_mod]) nt_pow_expr)) in
    pack nt (fun (e1, rest) ->
      List.fold_left (fun acc (op, e) ->
        match (op, e) with
        | (Mul, Percent e2) -> BinOp (PerOf, acc, e2)
        | (op, e) -> BinOp (op, acc, e)
     ) e1 rest) s pos

       

  and nt_unary_expr s pos =
     let nt_unary_minus_num =
       pack (caten (char '-') nt_num)
         (fun (_, Num n) -> Num (-n))
     in
     let nt_unary_minus_expr =
       pack (caten (char '-') nt_unary_expr)
         (fun (_, e) -> BinOp (Sub, Num 0, e))
     in
     let nt_unary_div =
       pack (caten (char '/') nt_unary_expr)
         (fun (_, e) -> BinOp (Div, Num 1, e))
     in
     let nt_paren_unary_minus =
       pack (caten nt_lparen (caten (char '-') (caten nt_expr nt_rparen)))
         (fun (_, (_, (e, _))) -> BinOp (Sub, Num 0, e))
     in
     let nt_paren_unary_div =
       pack (caten nt_lparen (caten (char '/') (caten nt_expr nt_rparen)))
         (fun (_, (_, (e, _))) -> BinOp (Div, Num 1, e))
     in
     let nt_unary =
       disj_list [nt_unary_minus_num; nt_paren_unary_minus; nt_paren_unary_div; nt_unary_minus_expr; nt_unary_div]
     in
     (disj nt_unary nt_postfix_expr) s pos

     
     
    and nt_pow_expr s pos =
     let nt = caten nt_unary_expr
       (maybe (caten nt_pow nt_pow_expr)) in
     pack nt (fun (e1, opt) ->
       match opt with
       | None -> e1
       | Some (_, e2) -> BinOp (Pow, e1, e2)) s pos

    

   
 
and nt_postfix_percent_expr s =
  let nt = caten nt_base_expr (star nt_percent) in
  pack nt (fun (e, percents) ->
    List.fold_left (fun acc _ -> Percent acc) e percents) s 
    

   
and nt_postfix_expr s pos =
  let nt = pack (caten nt_base_expr (caten (star nt_call_or_index) (star nt_percent))) (fun (e, (funcs, percents)) ->
    let expr_with_funcs = List.fold_left (fun acc f -> f acc) e funcs in
    List.fold_left (fun acc _ -> Percent acc) expr_with_funcs percents
  ) in
  nt s pos


 
 and nt_call_or_index s pos =
   let nt = disj nt_call nt_index in
   nt s pos
 

 and nt_call s pos =
   let nt = caten nt_lparen
     (caten (maybe (caten nt_expr (star (caten nt_comma nt_expr))))
             nt_rparen) in
   pack nt (fun (_, (args_opt, _)) ->
     let args = match args_opt with
       | None -> []
       | Some (first, rest) -> first :: List.map snd rest
     in
     (fun e -> Call (e, args))
   ) s pos

 
 and nt_index s pos =
   let nt = caten nt_lbracket (caten nt_expr nt_rbracket) in
   pack nt (fun (_, (idx, _)) ->
     (fun e -> Deref (e, idx))
   ) s pos
 

 and nt_base_expr s pos =
   (disj_list [nt_paren_expr; nt_num; nt_var]) s pos
   
 
 and nt_paren_expr s pos =
   let nt = caten nt_lparen (caten nt_expr nt_rparen) in
   pack nt (fun (_, (e, _)) -> e) s pos
 








 end;; (* module InfixParser *)
 
 open InfixParser;;