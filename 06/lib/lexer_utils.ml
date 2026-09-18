type location =
  { filename : string
  ; line : int
  ; col : int
  }

type error =
  { loc : location
  ; msg : string
  }

let curr_loc lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  { filename = pos.pos_fname; line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }
;;

let mk_error_loc loc msg = { loc; msg }
let mk_error lexbuf msg = mk_error_loc (curr_loc lexbuf) msg

let mk_error_lexeme lexbuf msg =
  let lexeme = Lexing.lexeme lexbuf in
  let new_msg = Printf.sprintf msg (String.escaped lexeme) in
  mk_error_loc (curr_loc lexbuf) new_msg
;;

let err_to_string e =
  let file_prefix = if e.loc.filename = "" then "" else e.loc.filename ^ ":" in
  Printf.sprintf "%s%d:%d: %s" file_prefix e.loc.line e.loc.col e.msg
;;
