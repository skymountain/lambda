{
  open Misc
  open Parser

  exception Lexical_error of string
  let err s = raise (Lexical_error ("Error: " ^ s))
    
  let reserv_words = [
    ("let",   Parser.LET);
    ("in",    Parser.IN);
    ("int",   Parser.INT);
    ("bool",  Parser.BOOL);
    ("true",  Parser.TRUE);
    ("false", Parser.FALSE);
    ("if",    Parser.IF);
    ("then",  Parser.THEN);
    ("else",  Parser.ELSE);
    ("rec",   Parser.REC);
    ("match", Parser.MATCH);
    ("with",  Parser.WITH);
    ("begin", Parser.BEGIN);
    ("end",   Parser.END);
    ("as",    Parser.AS);
    ("list",  Parser.LIST);
    ("of",    Parser.OF);
    ("type",  Parser.TYPE);
  ]
}

let lower_alphabet = ['a'-'z' '_']
let upper_alphabet = ['A'-'Z']
let alphabet       = lower_alphabet | upper_alphabet
let digit          = ['0'-'9']
let letter         = ['a'-'z' 'A'-'Z']
let symbolchar     = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ]

let tv_ident       = (letter | '_') (letter | digit | '_' | '\'')*
let lower_ident    = lower_alphabet (alphabet | digit | '\'')*
let upper_ident    = upper_alphabet (alphabet | digit | '\'')*
let blank          = [' ' '\009' '\012' '\n']

rule main = parse
  blank+ { main lexbuf }
| '\\'   { Parser.BACKSLA }
| '('    { Parser.LPAREN }
| ')'    { Parser.RPAREN }
| '.'    { Parser.DOT }
| '='    { Parser.EQ }
| ':'    { Parser.COLON }
| '['    { Parser.LSQPAREN }
| ']'    { Parser.RSQPAREN }
| '|'    { Parser.VBAR }
| ';'    { Parser.SEMICOLON }
| '_'    { Parser.UNDERBAR }
| ','    { Parser.COMMA }
| '\''   { Parser.QUOTE }
| "->"   { Parser.RARROW }
| ";;"   { Parser.SEMICOLON2 }
| "::"   { Parser.COLON2 }

| "-"? digit+
      {
        let s = Lexing.lexeme lexbuf in
        try Parser.INTLIT (int_of_string s) with
          Failure _ -> err @< Printf.sprintf "%s exceeds the range of integer" s
      }
      
| ['!' '~' '?'] symbolchar*
          { PREFIXOP (Lexing.lexeme lexbuf) }
| ['=' '<' '>' '|' '&' '$'] symbolchar*
          { INFIXOP0 (Lexing.lexeme lexbuf) }
| ['@' '^'] symbolchar*
          { INFIXOP1 (Lexing.lexeme lexbuf) }
| ['+' '-'] symbolchar*
          { INFIXOP2 (Lexing.lexeme lexbuf) }
| ['*' '/' '%'] symbolchar*
          { INFIXOP3 (Lexing.lexeme lexbuf) }
| "**" symbolchar*
          { INFIXOP4 (Lexing.lexeme lexbuf) }
| tv_ident as x
          { TVIDENT x }
| lower_ident
          {
            let s = Lexing.lexeme lexbuf in
            try List.assoc s reserv_words with
              Not_found -> LIDENT s
          }
| upper_ident
          { UIDENT (Lexing.lexeme lexbuf) }
| "(*"    { comment 0 lexbuf; main lexbuf }
| eof     { EOF }
| _       { err (Printf.sprintf "unknown token %s" @< Lexing.lexeme lexbuf) }
      
and comment depth = parse
    "*)" { if depth = 0 then () else comment (depth-1) lexbuf }
  | "(*" { comment (depth+1) lexbuf }
  | eof  { err "comment not terminated" }
  | _    { comment depth lexbuf }
