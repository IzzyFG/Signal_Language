(* Ocamllex scanner for Signal *)

{ open Signalparse }

let nonzerodigit    =  ['1'-'9']
let digit           =  ['0'-'9']
let bindigit        =  ('0' | '1')
let octdigit        =  ['0'-'7']
let hexdigit        =  digit | ['a'-'f'] | ['A'-'F']

let decinteger      =  digit*|nonzerodigit(['_']?digit)*
let bininteger      =  ['0']('b' | 'B')(['_']bindigit)+
let octinteger      =  ['0']('o' | 'O')(['_']octdigit)+
let hexinteger      =  ['0']('x' | 'X')(['_']hexdigit)+
let integer         =  decinteger | bininteger | octinteger | hexinteger

let digitpart     =  digit*|digit(['_']?digit)*
let exponent      =  ('e' | 'E')('+'|'-')?digitpart
let pointfloat    =  digitpart['.'](digitpart)?| (digitpart)?['.']digitpart
let exponentfloat =  (digitpart|pointfloat)exponent
let float         =  pointfloat | exponentfloat


let letter = ['a'-'z' 'A'-'Z']
let character = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n' '!' '#' '$' '%' '&' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' '\\' ']' '^' '_' '`' '{' '|' '}' '~']
let string = (character)*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACK }
| ']'      { RSQBRACK }
| ';'      { SEMI }
| '.'      { DOT }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULT }
| '/'      { DIV }
| '%'      {MODULO}
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT }
| "<="     { LE }
| ">="     { GE }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "do"     { DO }
| "while"  { WHILE }
| "for"    { FOR }

(* KEYWORDS *)
| "var"    { VAR }
| "func"   { FUNC }

(* RETURN *)
| "return" { RETURN }

(* TYPES *)
| "int"    { INT }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "char"   { CHAR }
| "string" { STRING }
| "float"  { FLOAT }
| "void"   { VOID }
| "list"   { LIST }

(* a rule to parse a char variable *)
| '\'' character '\'' as lem { CLIT (String.get lem 1) }

(* a rule to parse a string variable *)
| '"' string '"' as lem { SLIT (String.sub lem 1 (String.length lem - 2)) }

| integer as lem  { ILIT(int_of_string lem) }
| float as lem  { FLIT (float_of_string lem) }

(*Identifiers*)
| letter (digit | letter | '_')* as lem { ID(lem) }


| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
