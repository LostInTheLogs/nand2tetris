{
  include Lexer_utils

  type token = At | Eol | Number of int | Eof [@@deriving show]
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Eol }
| ['0'-'9']+ as digits { Number (int_of_string digits) }
| '@' { At }
| eof { Eof }
| _ as char { error lexbuf (UnexpectedCharacter char) }
