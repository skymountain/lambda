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
  ]
}

let alphabet  = ['a'-'z']
let ident_top = ['a'-'z' '_']
let ident_bdy = ['a'-'z' '_' '\'' '0'-'9']
let blank = [' ' '\009' '\012' '\n']

let symbolchar = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' ]
  
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
| "->"   { Parser.RARROW }
| ";;"   { Parser.SEMICOLON2 }
| "::"   { Parser.COLON2 }
      
| "-"? [ '0'-'9' ]+
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
          
| ident_top ident_bdy*
         {
           let s = Lexing.lexeme lexbuf in
           try List.assoc s reserv_words with
             Not_found -> IDENT s
         }
| "(*"   { comment 0 lexbuf; main lexbuf }
| eof    { EOF }
| _      { err (Printf.sprintf "unknown token %s" @< Lexing.lexeme lexbuf) }
      
and comment depth = parse
    "*)" { if depth = 0 then () else comment (depth-1) lexbuf }
  | "(*" { comment (depth+1) lexbuf }
  | eof  { err "comment not terminated" }
  | _    { comment depth lexbuf }
