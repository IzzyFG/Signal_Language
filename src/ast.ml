(* Abstract Syntax Tree and functions for printing it *)

type op =
  | Add
  | Sub
  | Mult
  | Div
  | Equal
  | Neq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | And
  | Not
  | Or
  | Mod

type typ = Int | Float | Bool | Char | String | Void | List of typ * int

type expr =
  | Var
  | Func
  | Literal of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Assign of string * expr
  | ListLit of expr list * string
  | Listget of string * expr
  | AssignList of string * expr * expr
  (* function call *)
  | Call of string * expr list

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  (* return *)
  | Return of expr

(* var int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp : typ;
  fname : string;
  formals : bind list;
  locals : bind list;
  body : stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | LessEq -> "<="
  | Greater -> ">"
  | GreaterEq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Mod -> "%"

let rec string_of_expr = function
  | Var -> "var"
  | Func -> "func"
  | Literal l -> string_of_int l
  | FloatLit l -> string_of_float l
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | CharLit c -> String.make 1 c
  | StringLit s -> "\"" ^ s ^ "\""
  | Id s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_op o ^ " " ^ string_of_expr e
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | Listget (v, p) -> v ^ "[" ^ string_of_expr p ^ "]"
  | AssignList (v, p, e) ->
      v ^ "[" ^ string_of_expr p ^ "]" ^ " = " ^ string_of_expr e
  | ListLit (vs, n) ->
      n ^ "=" ^ "{"
      ^ List.fold_left ( ^ ) ""
          (List.map (fun x -> x ^ ", ") (List.map string_of_expr vs))
      ^ "}"
  | Call (f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
  | Block stmts ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"
  | If (e, s1, s2) ->
      "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n"
      ^ string_of_stmt s2
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") do\n" ^ string_of_stmt s
  | For (e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; "
      ^ string_of_expr e3 ^ ") do\n" ^ string_of_stmt s

let rec string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Void -> "void"
  | List (t, s) -> "list " ^ string_of_typ t ^ " [" ^ string_of_int s ^ "]"

let string_of_vdecl (t, id) =
  string_of_expr Var ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"

(* fvdecl is needed for pretty printing of the function formals *)
let string_of_formals_vdecl (t, id) =
  string_of_expr Var ^ " " ^ string_of_typ t ^ " " ^ id

let string_of_fdecl fdecl =
  string_of_expr Func ^ " " ^ string_of_typ fdecl.rtyp ^ " " ^ fdecl.fname ^ "("
  ^ String.concat ", " (List.map string_of_formals_vdecl fdecl.formals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.locals)
  ^ String.concat "" (List.map string_of_stmt fdecl.body)
  ^ "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n"
  ^ String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_fdecl funcs)
