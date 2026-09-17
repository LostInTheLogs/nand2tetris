open Hackasm

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Lexer.token lexbuf in
      Printf.printf "%s\n%!" (Lexer.show_token result);
      if result = Lexer.Eof then exit 0
    done
  with
  | Lexer.LexError (err, loc) ->
    Printf.eprintf "Error: %s\n%!" (Lexer.err_to_string (err, loc));
    exit 1
;;
