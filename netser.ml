open Netser_types

let p_t = function
    | Prim p -> prim2str p
    | Identifier s -> s

let print_elem e =
    let p_l = function
        | Ast_int_literal i -> string_of_int i in
    let p_c = function
        | Count_literal i when i = 1 -> ""
        | Count_literal i -> Printf.sprintf "[%d]" i
        | Count_ident s -> Printf.sprintf "[%s]" s in
    match e with
        | Ast_literal (v, t) -> Printf.sprintf "%s:%s" (p_t t) (p_l v)
        | Ast_ident (Some s, t, c) -> Printf.sprintf "%s%s:%s" (p_t t) (p_c c) s
        | Ast_ident (None, t, c) -> Printf.sprintf "%s%s" (p_t t) (p_c c)

let ast2sexp = function (name, ast_tree) ->
    let rec inner = function
    | Ast_elem s -> print_elem s
    | Ast_product l -> Printf.sprintf "(%s)" (String.concat " " (List.map inner l))
    | Ast_sum l -> String.concat " | " (List.map inner l)
    | Ast_pound (t, e) -> Printf.sprintf "%s#(%s)" (p_t t) (inner e) in
    Printf.sprintf "(%s %s)" name (inner ast_tree)

let parse s =
    let s = Lexing.from_string s in
    Netser_parser.parse_sexpr (Netser_lexer.tokens) s

(* successive sums will be nested after the parse; convert to list *)
let rec inner_concat_sums = function
    | Ast_product l -> Ast_product (List.map inner_concat_sums l)
    | Ast_sum l -> Ast_sum (List.map inner_concat_sums (List.fold_left inner_concat_sums2 [] l))
    | e -> e
and inner_concat_sums2 accum = function
    | Ast_sum l -> List.fold_left inner_concat_sums2 accum l
    | Ast_product l -> (Ast_product (List.map inner_concat_sums l)) :: accum
    | e -> e::accum

let concat_sums = function (name, e) -> (name, inner_concat_sums e)

(* routine to convert Ast_ident (None, t)  into Ast_ident (Some <string>, t) *)
(* so we can have a name when deconstructing for writing *)
let fixup_elems = function (name, tree) ->
    let count = ref (-1) in
    let mklabel () = incr count; Printf.sprintf "%s%d" name !count in
    let rec elem = function
        | Ast_elem (Ast_ident (None, t, c)) -> Ast_elem (Ast_ident (Some (mklabel ()), t, c))
        | Ast_product l -> Ast_product (List.map elem l)
        | Ast_sum l -> Ast_sum (List.map elem l)
        | q -> q in
    (name, elem tree)

let initialize s =
    let parsetree = parse s in
    let stuff x = fixup_elems (concat_sums x) in
    let ast = List.map stuff parsetree in
    ast
