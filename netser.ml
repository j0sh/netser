open Netser_types

let p_t = function
    | Prim p -> prim2str p
    | Identifier s -> s

let p_l = function
    | Ast_int_literal i -> string_of_int i
    | Ast_char_literal c -> Printf.sprintf "'%s'" (Char.escaped c)
    | Ast_str_literal s -> Printf.sprintf "\"%s\"" (String.escaped s)

let p_l_t t = function
    | Ast_int_literal i as l when t = (Prim PRIM_INT32) -> (p_l l)^"l"
    | lit -> p_l lit

let print_elem e =
    let p_c = function
        | Count_literal i when i = 1 -> ""
        | Count_literal i -> Printf.sprintf "[%d]" i
        | Count_ident s -> Printf.sprintf "[%s]" s in
    match e with
        | Ast_literal (v, t) -> Printf.sprintf "%s:%s" (p_t t) (p_l_t t v)
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
    | PRIM_STRING -> "string"

let id2type s = s^"_t"

let ident_type = function
    | (None, _, _) -> raise (Failure "Empty ident; fixup?")
    | (_, Prim PRIM_CHAR, a) when (Count_literal 1) <> a -> "string"
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

let elem_decon = function
    | Ast_literal (_, _) -> ""
    | Ast_ident (None, _, _) -> raise (Failure "Empty ident; fixup?")
    | Ast_ident (Some s, _,_) -> s

let rec deconstruct_data = function
    | Ast_elem e -> elem_decon e
    | Ast_product p -> prod_decon p
    | _ -> ""
and prod_decon p =
    let data = List.map deconstruct_data p in
    let ne = nonempty data in
    let str = String.concat ", " ne in
    if List.length ne > 1 then Printf.sprintf "(%s)" str else str

let deconstruct name data =
    let d = deconstruct_data data in
    if "" <> d then Printf.sprintf "let %s = %s in\n" d name else d

let prim_writer = function
    | PRIM_UINT8 -> "Netser_io.write_u8"
    | PRIM_UINT16 -> "Netser_io.write_u16"
    | PRIM_INT -> "Netser_io.write_int"
    | PRIM_INT32 -> "Netser_io.write_i32"
    | PRIM_CHAR -> "Netser_io.write_char"
    | PRIM_FLOAT -> "Netser_io.write_float"
    | PRIM_STRING -> "Netser_io.write_string"

let id_writer s = s^"_writer"

let type_writer = function
    | Prim p -> prim_writer p
    | Identifier s -> id_writer s

let write_literal lit typ =
    let value = p_l_t typ lit in
    let writer_str = type_writer typ in
    Printf.sprintf "%s b %s" writer_str value

let write_ident name typ count =
    if (Prim PRIM_CHAR) = typ && (Count_literal 1) <> count then
        Printf.sprintf "Netser_io.write_string b %s" name
    else
    let writer_str = type_writer typ in
    match count with
    | Count_literal 1 -> Printf.sprintf "%s b %s" writer_str name
    | _ -> Printf.sprintf "List.map (%s b) %s" writer_str name

let write_elem = function
    | Ast_literal (a, b) -> write_literal a b
    | Ast_ident (Some a, b, c) -> write_ident a b c
    | Ast_ident (None, _, _) -> raise (Failure "Empty ident; fixup?")

let rec data_writer name = function
    | Ast_elem e -> write_elem e
    | Ast_product p -> String.concat ";\n" (List.map (data_writer name ) p)
    | Ast_sum s -> write_sum name s
    | Ast_pound (typ, p) -> write_pound name typ p

and write_sum name s =
    let cap = upcase name in
    let str = Printf.sprintf "match %s with\n" name in
    let gen_ctr i x =
        let w = data_writer name x in
        let d = deconstruct_data x in
        let str = Printf.sprintf "%s%d %s" cap i d in
        Printf.sprintf "%s -> %s" str w in
    let str = str ^ String.concat "\n| " (List.mapi gen_ctr s) in
    Printf.sprintf "(%s)" str

and write_pound name typ p =
    let calc_den = function
    | Identifier i -> raise (Failure (Printf.sprintf "Can't have ident %s as pound-type" i))
    | Prim PRIM_FLOAT -> raise (Failure "Can't have float as pound-type")
    | Prim PRIM_STRING -> raise (Failure "String pound-type unsupported")
    | Prim PRIM_UINT8 | Prim PRIM_CHAR -> 1
    | Prim PRIM_UINT16 -> 2
    | Prim PRIM_INT | Prim PRIM_INT32 -> 4 in
    let str = "let contents =\nlet b = Netser_io.create () in\n" in
    let str = str ^ (data_writer name p) in
    let str = str ^ ";\nNetser_io.contents b in\n" in
    let len = Printf.sprintf "(int_of_float (ceil (float_of_int (String.length contents)/.%d.0)))" (calc_den typ) in
    Printf.sprintf "%s%s b %s;\nNetser_io.write_string b contents" str (type_writer typ) len

let signature name written is_recursive =
    let t = id2type name in
    let prefix = if !written then "" else
        (let p = "let " ^ (if is_recursive then "rec " else "") in
        written := true; p) in
    let w = id_writer name in
    Printf.sprintf "%s %s b (%s:%s) =\n" prefix w name t

let print_writers types =
    let write_list l =
        let prefix_written = ref false in
        let is_rec = List.length l > 1 in
        let write (n, d) =
            let sig_line = signature n prefix_written is_rec in
            let decon_line = deconstruct n d in
            let write_block = data_writer n d in
            sig_line^decon_line^write_block in
        let strs = List.map write l in
        String.concat "\nand " strs in
    String.concat "\n\n" (List.map write_list types)

let initialize s =
    let parsetree = parse s in
    let stuff x = fixup_elems x in
    let ast = List.map stuff parsetree in
    let order = Netser_ordering.type_order ast in
    let s0 = print_type order in
    let s1 = print_writers order in
    Printf.sprintf "%s\n\n%s\n" s0 s1
