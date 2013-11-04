%{
    open Netser_types
%}

%token EOF RPAREN LPAREN
%token <int> NUM
%token <string> IDENT

%start parse_sexpr
%type <Netser_types.t> parse_sexpr

%%

parse_sexpr: sexpr EOF { $1 }
sexpr:
    | LPAREN sexpr* RPAREN { Cons $2 }
    | IDENT { Ident $1 }
    | NUM { Int_literal $1 }
