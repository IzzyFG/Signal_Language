/* Ocamlyacc parser for Signal */

%{open Ast%}


%token SEMI LPAREN RPAREN LBRACE RBRACE LSQBRACK RSQBRACK
%token AND OR NOT PLUS MINUS MULT DIV ASSIGN MODULO
%token EQ NEQ LT LE GT GE 

%token IF ELSE DO WHILE FOR
%token INT BOOL CHAR STRING FLOAT VOID
/* return, COMMA token */
%token VAR FUNC RETURN COMMA DOT
%token LIST
%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <char> CLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left MULT DIV MODULO
%right NOT

%%
/*TODO: add array stuff
  |	NEW type_tag bracket_args RBRACKET 	{ ArrayCreate(Datatype($2), List.rev $3) }
	| 	expr bracket_args RBRACKET		 	{ ArrayAccess($1, List.rev $2) } 
	| 	LPAREN expr RPAREN 					{ $2 } 
*/
program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* var int x; */
vdecl:
  VAR typ ID { ($2, $3) }

fvdecl:
  FUNC typ ID { ($2, $3) }

typ:
    INT    { Int   }
  | BOOL   { Bool  }
  | CHAR   { Char  }
  | STRING { String }
  | FLOAT  { Float }
  | VOID   { Void }
  | LIST typ LSQBRACK ILIT RSQBRACK {List($2,$4)}


/* fdecl */
fdecl:
  fvdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr ($1)      }
  | LBRACE stmt_list RBRACE                 { Block ($2) }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN DO stmt ELSE DO stmt    { If($3, $6, $9) }
  | WHILE LPAREN expr RPAREN DO stmt           { While ($3, $6)  }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN DO stmt { For ($3, $5, $7, $10) }
  /* return */
  | RETURN expr SEMI                        { Return ($2 )     }

expr:
    literals         { ($1)                    }
  | expr PLUS   expr { Binop($1, Add,   $3)    }
  | expr MINUS  expr { Binop($1, Sub,   $3)    }
  | expr MULT   expr { Binop($1, Mult, $3)     }
  | expr DIV    expr { Binop($1, Div, $3)      }
  | expr EQ     expr { Binop($1, Equal, $3)    }
  | expr NEQ    expr { Binop($1, Neq, $3)      }
  | expr LT     expr { Binop($1, Less,  $3)    }
  | expr LE     expr { Binop($1, LessEq,  $3)  } 
  | expr GT     expr { Binop($1, Greater, $3)  }
  | expr GE     expr { Binop($1, GreaterEq, $3)}
  | expr AND    expr { Binop($1, And,   $3)    }
  | expr OR     expr { Binop($1, Or,    $3)    }
  | expr MODULO expr {Binop($1, Mod,   $3)     }
  | NOT expr         { Unop (Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)          }
  /*| expr ASSIGN expr { Assign ($1, $3)         }*/
  | ID ASSIGN LSQBRACK vlist RSQBRACK {ListLit($4,$1)}
  | ID LSQBRACK expr RSQBRACK { Listget($1,$3) }
  | ID LSQBRACK expr RSQBRACK ASSIGN expr {AssignList($1,$3,$6)}
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

literals: 
    ILIT           { Literal($1)            }
  | FLIT             { FloatLit($1)           }
  | BLIT             { BoolLit($1)            }
  | CLIT             { CharLit ($1)            }
  | SLIT             { StringLit($1)          }
  | ID               { Id($1)                 }

vlist:
{[]}
|vlist COMMA expr {$1 @ [$3]}
|expr     { [$1] }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
