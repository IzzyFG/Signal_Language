(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/
*)

module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Signal" in

  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and char_t = L.i8_type context
  and float_t = L.double_type context
  and void_t = L.i32_type context in
  let string_t = L.pointer_type i8_t in

  (* Return the LLVM type for a Signal type *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Char -> char_t
    | A.String -> string_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.List (t, s) -> L.array_type (ltype_of_typ t) s
  in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init =
        match t with
        | A.Int -> L.const_int i32_t 0
        | A.Bool -> L.const_int i1_t 0
        | A.Void -> L.const_int void_t 0
        | A.Char -> L.const_int char_t 0
        | A.String -> L.const_pointer_null string_t
        | A.Float -> L.const_float float_t 0.0
        | A.List (_, _) -> L.const_int (ltype_of_typ t) 0
      in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let bool_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        StringMap.add n local_var m
      in

      let formals =
        List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))
      in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n =
      try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in
    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((t, e) : sexpr) =
      match e with
      | SVar -> L.const_int i32_t 0
      | SFunc -> L.const_int i32_t 0
      | SLiteral i -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c -> L.const_int char_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) ->
          let e' = build_expr builder e in
          ignore (L.build_store e' (lookup s) builder);
          e'
      (* Binop *)
      | SBinop ((t1, e1), bop, (t2, e2)) -> (
          let e1' = build_expr builder (t1, e1)
          and e2' = build_expr builder (t2, e2) in
          let build_int bop el1 el2 =
            (match bop with
            | A.Add -> L.build_add
            | A.Sub -> L.build_sub
            | A.Mult -> L.build_mul
            | A.Div -> L.build_sdiv
            | A.Mod -> L.build_srem
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Less -> L.build_icmp L.Icmp.Slt
            | A.LessEq -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.GreaterEq -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure "Operation not permitted for int type"))
              el1 el2 "itmp" builder
          in

          (* I love ARM *)
          let build_float bop el1 el2 =
            (match bop with
            | A.Add -> L.build_fadd
            | A.Sub -> L.build_fsub
            | A.Mult -> L.build_fmul
            | A.Div -> L.build_fdiv
            | A.Mod -> L.build_frem
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Less -> L.build_icmp L.Icmp.Slt
            | A.LessEq -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.GreaterEq -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure "Operation not permitted for float type"))
              el1 el2 "ftmp" builder
          in

          let build_bool bop el1 el2 =
            (match bop with
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Less -> L.build_icmp L.Icmp.Slt
            | A.LessEq -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.GreaterEq -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure "Operation not permitted for bool type"))
              el1 el2 "btmp" builder
          in

          match t with
          | Int -> build_int bop e1' e2'
          | Float -> build_float bop e1' e2' (* Bug #5 *)
          | Bool -> build_bool bop e1' e2'
          | _ -> raise (Failure "Operation not permitted")
          (* else if (fst (t1,e1)) = A.String then (match op with
                 A.Add     -> L.build_call str_concat_f [| e1' ; e2' |] "str_concat_f" builder
               | A.Equal   -> L.build_call str_eq_f [| e1' ; e2' |] "str_eq_f" builder
               | A.Neq     -> L.build_call str_neq_f [| e1' ; e2' |] "str_neq_f" builder
               | _         -> raise (Failure("https://comicsandmemes.com/wp-content/uploads/blank-meme-template-094-we-dont-do-that-here-black-panther.jpg"))
             ) *))
      (*SCall to print string types on the screen*)
      (* print ints *)
      | SCall ("printi", [ e ]) ->
          L.build_call printf_func
            [| int_format_str; build_expr builder e |]
            "printf" builder
      (* print booleans *)
      | SCall ("printb", [ e ]) ->
          L.build_call printf_func
            [| bool_format_str; build_expr builder e |]
            "printf" builder
      (* print chars *)
      | SCall ("printc", [ e ]) ->
          L.build_call printf_func
            [| char_format_str; build_expr builder e |]
            "printf" builder
      (* print strings *)
      | SCall ("prints", [ e ]) ->
          L.build_call printf_func
            [| string_format_str; build_expr builder e |]
            "printf" builder
      (* print floats *)
      | SCall ("printf", [ e ]) ->
          L.build_call printf_func
            [| float_format_str; build_expr builder e |]
            "printf" builder
      | SCall (f, args) ->
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          let result =
            match fdecl.srtyp with A.Void -> "" | _ -> f ^ "_result"
          in
          L.build_call fdef (Array.of_list llargs) result builder
      | SListget (v, idx) ->
          let tp = build_expr builder idx in
          let idx' = [| L.const_int i32_t 0; tp |] in
          let ref = L.build_gep (lookup v) idx' "" builder in
          L.build_load ref "" builder
      | SAssignList (v, idx, e) ->
          let tp = build_expr builder idx in
          let exp = build_expr builder e in
          let idx'' = [| L.const_int i32_t 0; tp |] in
          let ref = L.build_gep (lookup v) idx'' "" builder in
          ignore (L.build_store exp ref builder);
          exp
      | SListLit (t, vs, n) ->
          let tp = 0 in
          let vs_sexpr = List.map (fun a -> (t, a)) vs in
          let store_iter (arr, idx) li =
            let tmp_idx = [| L.const_int i32_t 0; L.const_int i32_t idx |] in
            let ref = L.build_gep (lookup arr) tmp_idx "" builder in
            let _, act = li in
            ignore (L.build_store (build_expr builder act) ref builder);
            (arr, idx + 1)
          in
          ignore
            (List.fold_left store_iter (n, tp)
               (List.map (fun a -> (t, a)) vs_sexpr));
          L.const_int i32_t 1
      | SUnop (op, (t1, e1)) -> (
          let e1' = build_expr builder (t1, e1) in
          let build_unop_bool op el1 =
            (match op with
            | A.Not -> L.build_not
            | _ ->
                raise (Failure "Not operation not permitted for the given type"))
              el1 "boolNot" builder
          in
          match t1 with
          | Bool -> build_unop_bool op e1'
          | _ ->
              raise (Failure "Not operation not permitted for the given type"))
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
      | SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e ->
          ignore (build_expr builder e);
          builder
      | SReturn e ->
          ignore
            (match fdecl.srtyp with
            | A.Void -> L.build_ret_void builder
            | _ -> L.build_ret (build_expr builder e) builder);
          builder
      | SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = build_expr builder predicate in

          let then_bb = L.append_block context "then" the_function in
          ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
          let else_bb = L.append_block context "else" the_function in
          ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in
          (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end;
          add_terminal (L.builder_at_end context else_bb) build_br_end;

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context end_bb
      | SWhile (predicate, body) ->
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          (* partial function *)
          ignore (build_br_while builder);
          let while_builder = L.builder_at_end context while_bb in
          let bool_val = build_expr while_builder predicate in

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal
            (build_stmt (L.builder_at_end context body_bb) body)
            build_br_while;

          let end_bb = L.append_block context "while_end" the_function in

          ignore (L.build_cond_br bool_val body_bb end_bb while_builder);
          L.builder_at_end context end_bb
      | SFor (init, predicate, iter, body) ->
          ignore (build_stmt builder (SExpr init));
          let new_stmt_body = SBlock [ body; SExpr iter ] in
          let new_while = SWhile (predicate, new_stmt_body) in
          let stmt_list = SBlock [ SExpr init; new_while ] in
          build_stmt builder stmt_list
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  the_module
