open Netser_types

let rec print_sexpr = function
      Ident s -> Printf.sprintf "%s" s
    | Int_literal i -> Printf.sprintf "%d" i
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
    | Int_literal i ->
        Ast_elem (Ast_literal ((Ast_int_literal i), (Prim PRIM_INT)))
    | Ident s -> Ast_elem (Ast_ident (None, s))

(* convert parsetree to ast *)
let pt2ast s : Netser_types.ast_name =
    let is_ident = function
        | Ident s -> true
        | _ -> false in
    let get_ident = function
        | Ident s -> s
        | _ -> raise (Failure "Could not get ident; syntax error") in
    match s with
    | Cons (h::t) when is_ident h ->
        (get_ident h), innerpt2ast (Cons t)
    | _ -> raise (Failure "Declaration mising a named identifier")


let initialize s =
    let parsetree = parse s in
    List.map pt2ast parsetree
