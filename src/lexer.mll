{
  open Misc
  open Parser

  exception Lexical_error of string
  let err s = raise (Lexical_error ("Error: " ^ s))
    
  let reserv_words = [
    ("let", Parser.LET);
    ("in", Parser.IN);
    ("int", Parser.INT);
    ("bool", Parser.BOOL);
    ("true", Parser.TRUE);
    ("false", Parser.FALSE);
    ("if", Parser.IF);
    ("then", Parser.THEN);
    ("else", Parser.ELSE);
  ]
}

let alphabet  = ['a'-'z']
let ident_top = ['a'-'z' '_']
let ident_bdy = ['a'-'z' '_' '\'']
let blank = [' ' '\009' '\012' '\n']
rule main = parse
  blank+ { main lexbuf }
| '\\'   { Parser.BACKSLA }
| '+'    { Parser.PLUS }
| '*'    { Parser.ASTER }
| '/'    { Parser.SLASH }
| '('    { Parser.LPAREN }
| ')'    { Parser.RPAREN }
| '.'    { Parser.DOT }
| '='    { Parser.EQ }
| ':'    { Parser.COLON }
| "->"   { Parser.RARROW }
| ";;"   { Parser.SEMICOLON2 }
| "-"? [ '0'-'9' ]+
      {
        let s = Lexing.lexeme lexbuf in
        try Parser.INTLIT (int_of_string s) with
          Failure _ -> err @< Printf.sprintf "%s exceeds the range of integer" s
      }
| ident_top ident_bdy*
         {
           let s = Lexing.lexeme lexbuf in
           try List.assoc s reserv_words with
             Not_found -> IDENT s
         }
| eof    { EOF }
| _      { err (Printf.sprintf "unknown token: %s" @< Lexing.lexeme lexbuf) }