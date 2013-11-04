open Netser_types

let rec print_sexpr = function
      Ident s -> Printf.sprintf "%s" s
    | Int_literal i -> Printf.sprintf "%d" i
    | Cons l -> Printf.sprintf "(%s)" (String.concat " " ((List.map print_sexpr l)))

let parse s =
    let s = Lexing.from_string s in
    Netser_parser.parse_sexpr (Netser_lexer.tokens) s
