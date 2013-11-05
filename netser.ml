open Netser_types

let rec print_sexpr sexpr =
    let p_t t =
        match t with
        | Prim p -> prim2str p
        | Identifier s -> s in
    let p_l = function
        | Ast_int_literal i -> string_of_int i in
    let print_elem = function
        | Ast_literal (v, t) -> Printf.sprintf "%s:%s" (p_t t) (p_l v)
        | Ast_ident (Some s, t) -> Printf.sprintf "%s:%s" (p_t t) s
        | Ast_ident (None, t) -> Printf.sprintf "%s" (p_t t) in
    match sexpr with
    | Elem s -> print_elem s
    | Cons l -> Printf.sprintf "(%s)" (String.concat " " ((List.map print_sexpr l)))

let parse s =
    let s = Lexing.from_string s in
    Netser_parser.parse_sexpr (Netser_lexer.tokens) s


let normalize l =
    (* converts (a b c (def) (ghi jkl) m n o) to
                ((a b c) (def) (ghi jkl) (m n o))     *)

    (* if see cons, create new product type, otherwise accum *)
    let acc = ref [] in
    let append x =
        if [] <> x then acc := (Cons (List.rev x)) :: !acc in
    let rec doit accum = function
    | (Cons q)::t ->
        append accum;
        ignore(doit [] q);
        doit [] t
    | h::t -> doit (h::accum) t
    | [] -> append accum; List.rev !acc in
    doit [] l

let rec innerpt2ast s =
    let is_product l =
        let is_cons = function
            | Cons _ -> true
            | _ -> false in
        [] = List.filter is_cons l in
    match s with
    | Cons (h::[]) -> innerpt2ast h
    | Cons l when is_product l -> Ast_product (List.map innerpt2ast l)
    | Cons l -> Ast_sum (List.map innerpt2ast (normalize l))
    | Elem e -> Ast_elem e

(* convert parsetree to ast *)
let pt2ast s : Netser_types.ast_name =
    let is_ident = function
        | Elem (Ast_ident (_, _)) -> true
        | _ -> false in
    let get_ident = function
        | Elem (Ast_ident (None, (Identifier s))) -> s
        | _ -> raise (Failure "Could not get ident; syntax error") in
    match s with
    | Cons (h::t) when is_ident h ->
        (get_ident h), innerpt2ast (Cons t)
    | _ -> raise (Failure "Declaration mising a named identifier")

(* routine to convert Ast_ident (None, t)  into Ast_ident (Some <string>, t) *)
(* so we can have a name when deconstructing for writing *)
let fixup_elems = function (name, tree) ->
    let count = ref (-1) in
    let mklabel () = incr count; Printf.sprintf "%s%d" name !count in
    let rec elem = function
        | Ast_elem (Ast_ident (None, t)) -> Ast_elem (Ast_ident (Some (mklabel ()), t))
        | Ast_product l -> Ast_product (List.map elem l)
        | Ast_sum l -> Ast_sum (List.map elem l)
        | q -> q in
    (name, elem tree)

let desugar s =
    let q = normalize (parse s) in
    let strings = List.map print_sexpr q in
    let str = String.concat " " strings in
    Printf.printf "%s\n" str

let initialize s =
    let parsetree = parse s in
    let stuff x = fixup_elems (pt2ast x) in
    let ast = List.map stuff parsetree in
    ast
