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
    let p = Printf.sprintf "(%s)" in
    let rec inner = function
    | Ast_elem s -> print_elem s
    | Ast_product l -> p (String.concat " " (List.map inner l))
    | Ast_sum l -> p (String.concat " | " (List.map inner l))
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
    | Ast_pound (_, e) -> deconstruct_data e
    | Ast_sum _ -> ""

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
    let str = "let contents =\nlet b = Netser_io.create () in\n" in
    let str = str ^ (data_writer name p) in
    let str = str ^ ";\nNetser_io.contents b in\n" in
    let conv = if Prim PRIM_INT32 = typ then "(Int32.of_int (String.length contents))" else "(String.length contents)" in
    Printf.sprintf "%s%s b %s;\nNetser_io.write_string b contents" str (type_writer typ) conv

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

let type_list types =
    let a l = List.fold_left (fun acc (n, d) -> n::acc) [] l in
    List.fold_left (fun acc l -> acc@(a l)) [] types

let print_exns types =
    let names = type_list types in
    let gen_exn s =
        let idt = id2type s in
        (upcase idt)^"_exn of "^idt in
    let exn_strs = List.map gen_exn names in
    let univ = String.concat " | " exn_strs in
    "type universal_type = "^univ^"
    exception Sum_match of universal_type
    exception Invalid_read"

let prim_reader = function
    | PRIM_UINT8 -> "Netser_io_reader.read_u8"
    | PRIM_UINT16 -> "Netser_io_reader.read_u16"
    | PRIM_INT32 -> "Netser_io_reader.read_int32"
    | PRIM_INT -> "Netser_io_reader.read_int"
    | PRIM_FLOAT -> "Netser_io_reader.read_float"
    | PRIM_CHAR -> "Netser_io_reader.read_char"
    | PRIM_STRING -> "Netser_io_reader.read_string"

let id_reader s = s^"_reader"

let type_reader = function
    | Prim p -> prim_reader p
    | Identifier s -> id_reader s

let count_str = function
    | Count_literal i -> string_of_int i
    | Count_ident s -> s

let read_ident name typ count =
    if (Prim PRIM_CHAR = typ && Count_literal 1 <> count ) then
    Printf.sprintf "let %s = %s b %s" name (prim_reader PRIM_STRING) (count_str count) else if Count_literal 1 = count then
    Printf.sprintf "let %s = %s b" name (type_reader typ) else
    Printf.sprintf "let acc = ref [] in
for i = 0 to %s do
    acc := (%s b) :: !acc
done;
let %s = !acc" (count_str count) (type_reader typ) name

let read_literal lit typ =
    let len = if Prim PRIM_STRING = typ then
        let p = match lit with Ast_str_literal s -> s
                | _ -> raise (Failure "String literal, non-string prim") in
        Printf.sprintf " %d" (String.length p)
    else "" in
    Printf.sprintf "let () = if %s <> (%s b%s) then raise Invalid_read" (p_l_t typ lit) (type_reader typ) len

let read_elem name = function
    | Ast_ident (None, _, _) -> raise (Failure "Empty ident; fixup?")
    | Ast_ident (Some s, t, c) -> read_ident s t c
    | Ast_literal (l, t) -> read_literal l t

let rec gen_data_reader name = function
    | Ast_elem e -> read_elem name e
    | Ast_product p -> String.concat " in\n" (List.map (gen_data_reader name) p)
    | Ast_pound (t, e) -> read_pound name t e
    | Ast_sum s -> read_sum name s

and read_pound name typ exp =
    Printf.sprintf "let len = %s b in\n%s" (type_reader typ) (gen_data_reader name exp)

and read_sum name exps =
    let gen_wrap i x =
        let str = gen_data_reader name x in
        let d = deconstruct_data x in
        let trailer = Printf.sprintf "%s%d %s" (upcase name) i d in
        Printf.sprintf "(fun b -> %s in %s)" str trailer in
    let name_exn = (upcase (id2type name))^"_exn" in
    let l = List.mapi gen_wrap exps in
    let g = Printf.sprintf "let %s_opts = [%s] in\n" name (String.concat ";\n" l) in
    let h = Printf.sprintf "let res = try
let pos = Netser_io_reader.pos b in
let g f = try
    let h = f b in
    raise (Sum_match (%s h))
with Invalid_read -> Netser_io_reader.set_pos b pos in
List.iter g %s_opts;
None
with Sum_match (%s h) -> Some h in
match res with
| Some s -> s
| None -> raise Invalid_read" name_exn name name_exn in
    g^h

let rec trail n d =
    let a = deconstruct_data d in
    if "" = a then
        match d with
        | Ast_sum e -> "" (*n*)
        | Ast_pound (_, e) -> trail n e
        | _ -> "in "^upcase n
    else "in "^a

let read_signature name written is_recursive =
    let prefix = if !written then "" else
        (let p = "let " ^ (if is_recursive then "rec " else "") in
        written := true; p) in
    let w = id_reader name in
    Printf.sprintf "%s %s b =\n" prefix w

let print_readers types =
    let write_readers l =
        let prefix_written = ref false in
        let is_rec = List.length l > 1 in
        let read (n, d) =
            let sig_line = read_signature n prefix_written is_rec in
            let body = gen_data_reader n d in
            let trailer = trail n d in
            Printf.sprintf "%s%s\n%s\n" sig_line body trailer in
        let strs = List.map read l in
        String.concat "\nand " strs in
    String.concat "\n" (List.map write_readers types)

let initialize s =
    let parsetree = parse s in
    let stuff x = fixup_elems x in
    let ast = List.map stuff parsetree in
    let order = Netser_ordering.type_order ast in
    let s0 = print_type order in
    let s1 = print_writers order in
    Printf.sprintf "%s\n\n%s\n" s0 s1
