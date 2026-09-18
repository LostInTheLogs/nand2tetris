type token =
  | At
  | Number of int
  | Symbol of string
  | LParen
  | RParen
  | Equals
  | Semicolon
  | Eol
  | Eof
  | UnexpectedCharacter of char
[@@deriving show]

type location =
  { filename : string
  ; line : int
  ; col : int
  ; lexeme : string
  }

type located =
  { token : token
  ; loc : location
  }

type error =
  { loc : location
  ; msg : string
  }

let curr_loc lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let lexeme = Lexing.lexeme lexbuf in
  { filename = pos.pos_fname
  ; line = pos.pos_lnum
  ; col = pos.pos_cnum - pos.pos_bol + 1
  ; lexeme
  }
;;

let mk_error_loc loc msg = { loc; msg }
let mk_error lexbuf msg = mk_error_loc (curr_loc lexbuf) msg

let err_to_string e =
  let loc = e.loc in
  let msg = String.replace_all ~sub:"%s" ~by:loc.lexeme e.msg in
  let file_prefix = if loc.filename = "" then "" else loc.filename ^ ":" in
  Printf.sprintf "%s%d:%d: %s" file_prefix loc.line loc.col msg
;;
