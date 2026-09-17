type location =
  { filename : string
  ; line : int
  ; col : int
  }
[@@deriving show]

type error_kind = UnexpectedCharacter of char [@@deriving show]

exception LexError of error_kind * location

let curr_loc lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  { filename = pos.pos_fname; line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }
;;

let error_loc loc e = raise (LexError (e, loc))
let error lexbuf e = error_loc (curr_loc lexbuf) e

let err_to_string (e, loc) =
  let file_prefix = if loc.filename = "" then "" else loc.filename ^ ":" in
  let msg =
    match e with
    | UnexpectedCharacter c -> Printf.sprintf "Illegal character '%c'" c
  in
  Printf.sprintf "%sLine %d, Column %d: %s" file_prefix loc.line loc.col msg
;;
