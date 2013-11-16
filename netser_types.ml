type prim_t = PRIM_UINT8 | PRIM_UINT16 | PRIM_INT32 | PRIM_INT | PRIM_FLOAT | PRIM_CHAR | PRIM_STRING

type ast_type = Prim of prim_t | Identifier of string
type ast_literal = Ast_int_literal of int | Ast_char_literal of char | Ast_str_literal of string
type ast_count = Count_literal of int | Count_ident of string
type ast_elem = Ast_literal of ast_literal * ast_type | Ast_ident of string option * ast_type * ast_count
type ast = Ast_elem of ast_elem | Ast_product of ast list | Ast_sum of ast list | Ast_pound of ast_type * ast
type ast_name = string * ast

let prim2str = function
    | PRIM_UINT8 -> "uint8"
    | PRIM_UINT16 -> "uint16"
    | PRIM_INT32 -> "int32"
    | PRIM_INT -> "int"
    | PRIM_FLOAT -> "float"
    | PRIM_CHAR -> "char"
    | PRIM_STRING -> "string"

let str2prim = function
    | "uint8" -> PRIM_UINT8
    | "uint16" -> PRIM_UINT16
    | "int32" -> PRIM_INT32
    | "int" -> PRIM_INT
    | "float" -> PRIM_FLOAT
    | "char" -> PRIM_CHAR
    | "string" -> PRIM_STRING
    | _ -> raise (Failure "Unknown primitive")
