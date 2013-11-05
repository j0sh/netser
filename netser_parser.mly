%{
    open Netser_types

    let mkliteral num kind =
        let lit = Ast_int_literal num in
        Ast_literal (lit, kind)
    let mkident name kind =
        Ast_ident (Some name, kind)
    let infer_type x =
        if x < 256 then Prim PRIM_UINT8
        else if x < 65536 then Prim PRIM_UINT16
        else Prim PRIM_INT
%}

%token EOF COLON PIPE RPAREN LPAREN
%token <int> NUM
%token <string> IDENT
%token <Netser_types.prim_t> PRIM

%start parse_sexpr
%type <Netser_types.sexp_t list> parse_sexpr

%%

parse_sexpr: sexpr* EOF { $1 }
sexpr:
    | LPAREN sexpr* RPAREN { Cons $2 }
    | typexpr { Elem $1 }
    | PIPE { Cons [] }

type_kind: IDENT { Identifier $1} | PRIM { Prim $1 }

typexpr:
    | type_kind COLON NUM { mkliteral $3 $1 }
    | type_kind COLON IDENT { Ast_ident (Some $3, $1) }
    | NUM { mkliteral $1 (infer_type $1) }
    | type_kind { Ast_ident (None, $1) }
