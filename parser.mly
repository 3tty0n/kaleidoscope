%{
    open Ast
    open Lexing
    let parse_error s =
      begin
        try
          let start_pos = Parsing.symbol_start_pos ()
          and end_pos = Parsing.symbol_end_pos () in
          Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
            start_pos.pos_fname
            start_pos.pos_lnum
            (start_pos.pos_cnum - start_pos.pos_bol)
            (end_pos.pos_cnum - start_pos.pos_bol)
        with Invalid_argument(_) -> ()
      end;
      Printf.printf "Syntax error: %s\n" s;
      raise Parsing.Parse_error
%}

/* lexer tokens */

%token DEF EXTERN
%token <string> IDENT
%token <float> NUMBER
%token LPAREN RPAREN
%token COMMA SEMICOLON COLON
%token EOF
%token UNKNOWN
%token LESS_THAN PLUS MINUS TIMES
%token IF THEN ELSE


/* parser */

%left LESS_THAN
%left PLUS MINUS
%left TIMES


%start toplevel
%type <Ast.toplevel> toplevel
%%

expr:
    | NUMBER { Number $1 }
    | IDENT  { Variable $1 }
    | LPAREN expr RPAREN { $2 }
    | LPAREN expr { parse_error "expected `)'"}
    | IDENT LPAREN argumentexpr RPAREN { Call($1, Array.of_list $3) }
    | IDENT LPAREN RPAREN              { Call($1, Array.of_list []) }
    | IDENT LPAREN argumentexpr        { parse_error "expected `)'"}
    | expr LESS_THAN expr              { Binary('<', $1, $3) }
    | expr PLUS expr                   { Binary('+', $1, $3) }
    | expr MINUS expr                  { Binary('-', $1, $3) }
    | expr TIMES expr                  { Binary('*', $1, $3) }
    | UNKNOWN                          { parse_error "unknown token when expecting an expression." }

    ;

argumentexpr:
    | expr COMMA argumentexpr          { $1 :: $3 }
    | expr                             { [$1] }
    ;

/* prototype
 *   ::= id '(' id* ')' */
prototype:
  | IDENT LPAREN idents RPAREN       { Prototype($1, Array.of_list $3) }
  | IDENT LPAREN idents              { parse_error "expected ')' in prototype" }
  | IDENT                            { parse_error "expected '(' in prototype" }
;
idents:
  | IDENT idents                     { $1 :: $2 }
  |                                  { [] }
;

/*  external ::= 'extern' prototype */
extern:
  | EXTERN prototype                 { $2 }
;

/* definition ::= 'def' prototype expression */
definition:
  | DEF prototype expr               { Function($2, $3) }
;

toplevel:
  | statement terminator             { $1 }
  | terminator                       { $1 }
;

statement:
  | expr                             { Expression($1) }
  | extern                           { Extern $1 }
  | definition                       { Definition $1 }
 ;
terminator:
  | SEMICOLON                        { Sep }
  | EOF                              { End }
;
