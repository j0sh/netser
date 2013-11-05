{
    open Netser_parser
}

let upper = ['A'-'Z']
let lower = ['a'-'z']
let digit = ['0'-'9']
let blank = [' ' '\t']
let newline = '\r'? '\n'
let numeric = digit+
let ident = lower (lower | upper | digit | '_')*

rule tokens = parse
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | '|'           { PIPE }
    | ident as s    { IDENT s }
    | numeric as n  { NUM (int_of_string n) }
    | blank+        { tokens lexbuf }
    | newline       { Lexing.new_line lexbuf; tokens lexbuf }
    | eof           { EOF }
