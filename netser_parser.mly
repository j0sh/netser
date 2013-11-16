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
    let simplify_typexpr = function
    | Ast_elem h :: [] -> Ast_elem h
    | Ast_pound (t, e) :: [] -> Ast_pound (t, e)
    | [] -> Ast_product []
    | (h::t) as l -> Ast_product l
%}

%token EOF TYPE EQUAL POUND COLON PIPE RPAREN LPAREN LBRACK RBRACK LCURL RCURL
%token <int> NUM
%token <char> CHAR
%token <string> STRING IDENT
%token <Netser_types.prim_t> PRIM

%start parse_sexpr
%type <Netser_types.ast_name list> parse_sexpr

%%

parse_sexpr: ast* EOF { $1 }
ast:
    | TYPE IDENT EQUAL expr { ($2, $4) }

expr:
    | typexpr { Ast_elem $1 }
    | LCURL expr* RCURL { simplify_typexpr $2 }
    | LPAREN g = separated_list(PIPE, expr) RPAREN { Ast_sum g }
    | type_kind POUND expr { Ast_pound ($1, $3) }

type_kind: IDENT { Identifier $1} | PRIM { Prim $1 }
index: IDENT { Count_ident $1 } | NUM { Count_literal $1 }

typexpr:
    | type_kind COLON NUM { mkliteral $3 $1 }
    | type_kind COLON IDENT { mkident (Some $3) $1 }
    | type_kind LBRACK index RBRACK { Ast_ident (None, $1, $3) }
    | type_kind LBRACK index RBRACK COLON IDENT { Ast_ident (Some $6, $1, $3) }
    | NUM { mkliteral $1 (infer_type $1) }
    | CHAR { Ast_literal ((Ast_char_literal $1), (Prim PRIM_CHAR)) }
    | STRING { Ast_literal ((Ast_str_literal $1), (Prim PRIM_STRING)) }
    | type_kind { mkident None $1 }
