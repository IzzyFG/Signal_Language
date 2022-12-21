(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
  | SVar
  | SFunc
  | SLiteral of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SUnop of op * sexpr
  | SListLit of typ * sx list * string
  | SListget of string * sexpr
  | SAssignList of string * sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt list;
}

type sprogram = bind list * sfunc_def list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ (match e with
    | SVar -> "var"
    | SFunc -> "func"
    | SLiteral l -> string_of_int l
    | SFloatLit l -> string_of_float l
    | SBoolLit true -> "true"
    | SBoolLit false -> "false"
    | SCharLit l -> Char.escaped l
    | SStringLit l -> l
    | SId s -> s
    | SBinop (e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SListLit (t, vs, n) ->
        n ^ "=" ^ "{"
        ^ List.fold_left ( ^ ) ""
            (List.map
               (fun x -> x ^ ", ")
               (List.map string_of_sexpr (List.map (fun x -> (t, x)) vs)))
        ^ "}"
    | SListget (v, p) -> v ^ "[" ^ string_of_sexpr p ^ "]"
    | SAssignList (v, p, e) ->
        v ^ "[" ^ string_of_sexpr p ^ "]" ^ " = " ^ string_of_sexpr e
    | SAssign (v, e) -> v ^ " = " ^ string_of_sexpr e
    | SUnop (o, e1) -> string_of_op o ^ " " ^ string_of_sexpr e1
    | SCall (f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")")
  ^ ")"

let rec string_of_sstmt = function
  | SBlock stmts ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr expr -> string_of_sexpr expr ^ ";\n"
  | SReturn expr -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf (e, s1, s2) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n"
      ^ string_of_sstmt s2
  | SWhile (e, s) ->
      "while (" ^ string_of_sexpr e ^ ") do\n" ^ string_of_sstmt s
  | SFor (e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ "; "
      ^ string_of_sexpr e3 ^ ") do\n" ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_expr Func ^ " " ^ string_of_typ fdecl.srtyp ^ " "
  (* NOT SURE ABOUT THIS LINE*)
  ^ fdecl.sfname
  ^ "("
  ^ String.concat ", " (List.map string_of_formals_vdecl fdecl.sformals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.slocals)
  ^ String.concat "" (List.map string_of_sstmt fdecl.sbody)
  ^ "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n"
  ^ String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sfdecl funcs)
