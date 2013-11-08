%{
    open Netser_types

    let mkliteral num kind =
        let lit = Ast_int_literal num in
        Ast_literal (lit, kind)
    let mkident name kind =
        Ast_ident (name, kind, Count_literal 1)
    let infer_type x =
        if x < 256 then Prim PRIM_UINT8
        else if x < 65536 then Prim PRIM_UINT16
        else Prim PRIM_INT
%}

%token EOF COLON PIPE RPAREN LPAREN LBRACK RBRACK
%token <int> NUM
%token <string> IDENT
%token <Netser_types.prim_t> PRIM

%start parse_sexpr
%type <Netser_types.ast_name list> parse_sexpr

%%

parse_sexpr: ast* EOF { $1 }
ast:
    | LPAREN IDENT expr RPAREN { ($2, $3) }

te: typexpr* { Ast_product (List.map (fun x -> Ast_elem x) $1) }

expr:
    | te { $1 }
    | te PIPE expr { Ast_sum ($1::[$3]) }

type_kind: IDENT { Identifier $1} | PRIM { Prim $1 }
index: IDENT { Count_ident $1 } | NUM { Count_literal $1 }

typexpr:
    | type_kind COLON NUM { mkliteral $3 $1 }
    | type_kind COLON IDENT { mkident (Some $3) $1 }
    | type_kind LBRACK index RBRACK { Ast_ident (None, $1, $3) }
    | type_kind LBRACK index RBRACK COLON IDENT { Ast_ident (Some $6, $1, $3) }
    | NUM { mkliteral $1 (infer_type $1) }
    | type_kind { mkident None $1 }
