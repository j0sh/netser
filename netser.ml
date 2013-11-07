open Netser_types

module SS = Set.Make(String)

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


let rec tree_deps = function
    | Ast_elem (Ast_ident(_, Identifier s, _)) -> [s]
    | Ast_product l -> List.flatten (List.map tree_deps l)
    | Ast_sum l -> List.flatten (List.map tree_deps l)
    | _ -> []

let outgoing_edges s =
    let trees = List.map (fun (_, tree) -> tree) s in
    List.map tree_deps trees

let incoming_edges l =
    let incoming : (string, string list) Hashtbl.t = Hashtbl.create 100 in
    let outgoings = outgoing_edges l in
    let set_empty (name, _) = Hashtbl.add incoming name [] in
    let add2in (name, _) edges =
        let add2hash x =
            try let s = Hashtbl.find incoming x in
            Hashtbl.replace incoming x (name::s)
            with exn -> () in (* occurs with undefined types *)
        List.iter add2hash edges in
    let in2list (name, _) = Hashtbl.find incoming name in
    List.iter set_empty l;
    List.iter2 add2in l outgoings;
    List.map in2list l

let dep_order l =
    (* create hashmap of name -> edges *)
    let out : (string, string list) Hashtbl.t = Hashtbl.create 100 in
    let edges = outgoing_edges l in
    List.iter2 (fun (name, _) e -> Hashtbl.add out name e) l edges;

    let add s value = s := SS.add value !s in
    let remove s value = s := SS.remove value !s in
    let contains s value = SS.exists (fun x -> x = value) !s in

    let marked = ref SS.empty in
    let temp = ref SS.empty in
    let unmarked = ref SS.empty in
    let order = ref [] in

    (* topological sort *)
    List.iter (fun (name, _) -> (add unmarked name)) l;
    let rec visit name =
        if contains temp name then raise (Failure "Not a DAG");
        if not (contains marked name) then begin
            try
                let n_edges = Hashtbl.find out name in
                add temp name;
                List.iter visit n_edges;
                remove temp name;
                add marked name;
                order := name :: !order
            with Not_found -> (); (* nonexistent name, what to do? *)
        end in
    while not (SS.is_empty !unmarked) do
        let name = SS.choose !unmarked in
        remove unmarked name;
        visit name
    done;
    List.rev !order

let find_cycles trees =
    (* create hashmap of name -> edges *)
    let out : (string, string list) Hashtbl.t = Hashtbl.create 100 in
    let edges = outgoing_edges trees in
    List.iter2 (fun (name, _) e -> Hashtbl.add out name e) trees edges;

    let add s value = s := SS.add value !s in
    let remove s value = s := SS.remove value !s in
    let contains s value = SS.exists (fun x -> x = value) !s in

    let unvisited = ref SS.empty in
    let cycles = ref [] in

    let rec inner l =
        let check_neighbor x =
            (* check cycle by looking for an existing x in list *)
            if [] <> List.filter (fun y -> x = y) l then begin
                let found = ref false in
                (* list could be 1 2 3 4 1 5 6; trim to 1 2 3 4 *)
                let trim_list accum y  =
                    let a = if not !found then y::accum else accum in
                    if not !found then found := y = x;
                    a in
                let cycle = List.fold_left trim_list [] l in
                cycles := cycle :: !cycles;
            end in
        let x = List.hd l in
        remove unvisited x;
        let neighbors = Hashtbl.find out x in
        List.iter check_neighbor neighbors;
        (* now recurse to unvisited nodes *)
        let is_unvisited = contains unvisited in
        let neighbors = List.filter is_unvisited neighbors in
        List.iter (fun n -> inner (n::l)) neighbors in

    List.iter (fun (name, _) -> add unvisited name) trees;
    while not (SS.is_empty !unvisited) do
        let name = SS.choose !unvisited in
        inner [name];
    done;
    !cycles

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
