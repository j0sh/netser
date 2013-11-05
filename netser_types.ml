type prim_t = PRIM_UINT8 | PRIM_UINT16 | PRIM_INT32 | PRIM_INT | PRIM_FLOAT | PRIM_CHAR

type ast_type = Prim of prim_t | Identifier of string
type ast_literal = Ast_int_literal of int
type ast_elem = Ast_literal of ast_literal * ast_type | Ast_ident of string option * ast_type
type ast = Ast_elem of ast_elem | Ast_product of ast list | Ast_sum of ast list
type ast_name = string * ast

type sexp_t = Elem of ast_elem | Cons of sexp_t list

let prim2str = function
    | PRIM_UINT8 -> "uint8"
    | PRIM_UINT16 -> "uint16"
    | PRIM_INT32 -> "int32"
    | PRIM_INT -> "int"
    | PRIM_FLOAT -> "float"
    | PRIM_CHAR -> "char"

let str2prim = function
    | "uint8" -> PRIM_UINT8
    | "uint16" -> PRIM_UINT16
    | "int32" -> PRIM_INT32
    | "int" -> PRIM_INT
    | "float" -> PRIM_FLOAT
    | "char" -> PRIM_CHAR
    | _ -> raise (Failure "Unknown primitive")
