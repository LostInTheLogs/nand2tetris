{ include Lexer_utils }

let char = ['a'-'z' 'A'-'Z']

let digit = ['0'-'9']

let symbol_beg = (char | ['_' '.' '$' ':'])

let symbol_part =
  (char | digit | ['_' '.' '$' ':' '+' '-' '&' '!' '|'])

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; Eol }
| '@' { At }
| '(' { LParen }
| ')' { RParen }
| '=' { Equals }
| ';' { Semicolon }
| digit+ as digits { Number (int_of_string digits) }
| symbol_beg symbol_part* as name { Symbol name }
| eof { Eof }
| _ as char { UnexpectedCharacter char }
