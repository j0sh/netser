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
    | Ast_pound (t, e) -> Ast_pound (t, inner_concat_sums e)
    | e -> e
and inner_concat_sums2 accum = function
    | Ast_sum l -> List.fold_left inner_concat_sums2 accum l
    | Ast_product l -> (Ast_product (List.map inner_concat_sums l)) :: accum
    | Ast_pound (t, e) -> Ast_pound (t, inner_concat_sums e)::accum
    | e -> e::accum

let concat_sums = function (name, e) -> (name, inner_concat_sums e)

(* routine to convert Ast_ident (None, t)  into Ast_ident (Some <string>, t) *)
(* so we can have a name when deconstructing for writing *)
let fixup_elems = function (name, tree) ->
    let count = ref (-1) in
    let mklabel () = incr count; Printf.sprintf "%s%d" name !count in
    let rec elem = function
        | Ast_elem (Ast_ident (None, t, c)) -> Ast_elem (Ast_ident (Some (mklabel ()), t, c))
        | Ast_elem _ as s -> s
        | Ast_product l -> Ast_product (List.map elem l)
        | Ast_sum l -> Ast_sum (List.map elem l)
        | Ast_pound (typ, e) -> Ast_pound (typ, elem e) in
    (name, elem tree)

let prim2type = function
    | PRIM_UINT8 | PRIM_UINT16 | PRIM_INT  -> "int"
    | PRIM_INT32 -> "int32"
    | PRIM_FLOAT -> "float"
    | PRIM_CHAR -> "char"

let id2type s = s^"_t"

let ident_type = function
    | (None, _, _) -> raise (Failure "Empty ident; fixup?")
    | (_, Prim p, Count_literal 1) -> prim2type p
    | (_, Prim p, _) -> (prim2type p) ^ " list"
    | (_, Identifier s, Count_literal 1) -> id2type s
    | (_, Identifier s, _) -> (id2type s) ^ " list"

let elem_type = function
    | Ast_literal (_, _) -> ""
    | Ast_ident (a, b, c) -> ident_type (a, b, c)

let upcase str =
    let s = String.copy str in
    s.[0] <- Char.uppercase s.[0];
    s

let nonempty = List.filter (fun x -> x <> "")

let rec data_type name = function
    | Ast_elem e -> elem_type e
    | Ast_product p -> String.concat " * " (nonempty (List.map (data_type name) p))
    | Ast_sum s -> String.concat " | " (sum_type name s)
    | Ast_pound (_, e) -> data_type name e

and sum_type name l =
    let cap = upcase name in
    let ne = List.map (data_type name) l in
    let gen_ctr i x =
        (* don't append 'of ...' for empty types *)
        let str = Printf.sprintf "%s%d" cap i in
        if "" <> x then Printf.sprintf "%s of %s" str x else str in
    List.mapi gen_ctr ne

let print_type types =
    let pn s = (id2type s)^" = " in
    let gen_types l =
        let get_type name data =
            let typed_s = data_type name data in
            if "" = typed_s then upcase name else typed_s in
        let gen_type (name, data) = (pn name)^(get_type name data) in
        let m = List.map gen_type l in
        let str = String.concat "\nand " m in
        "type "^str in
    let strs = List.map gen_types types in
    String.concat "\n\n" strs

let initialize s =
    let parsetree = parse s in
    let stuff x = fixup_elems (concat_sums x) in
    let ast = List.map stuff parsetree in
    let order = Netser_ordering.type_order ast in
    print_type order
