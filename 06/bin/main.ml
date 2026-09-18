open Hackasm

type dest =
  | M
  | D
  | MD
  | A
  | AM
  | AD
  | AMD
[@@deriving show]

type comp =
  | Zero
  | One
  | D_op
  | A_op
[@@deriving show]

type jump =
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
[@@deriving show]

type symbol_or_value =
  | Literal of int
  | Symbol of string
[@@deriving show]

type instruction =
  | A_instr of symbol_or_value
  | C_instr of
      { dest : dest option
      ; comp : comp
      ; jump : jump option
      }
  | T_instr of
      { dest : Lexer.token option
      ; comp : Lexer.token
      ; jump : Lexer.token option
      }
  | Label of string
[@@deriving show]

type program = instruction list [@@deriving show]

let ( let* ) = Result.bind
let ( let+ ) r f = Result.map f r

let rec parse lexbuf acc =
  let token = Lexer.token lexbuf in
  match token with
  | Eol -> parse lexbuf acc
  | Eof -> Ok (List.rev acc)
  | At ->
    let* instr = parse_a_instr lexbuf in
    parse lexbuf (instr :: acc)
  | other ->
    let* instr = parse_c_instr lexbuf other in
    parse lexbuf (instr :: acc)

and expect_eol_or_eof lexbuf =
  match Lexer.token lexbuf with
  | Eol | Eof -> Ok ()
  | _ -> Error (Lexer.mk_error lexbuf "Expected end of line, got: '%s'")

and parse_a_instr lexbuf =
  let* res =
    match Lexer.token lexbuf with
    | Number num -> Ok (A_instr (Literal num))
    | Symbol sym -> Ok (A_instr (Symbol sym))
    | _ -> Error (Lexer.mk_error lexbuf "Unexpected token: '%s'")
  in
  let* () = expect_eol_or_eof lexbuf in
  Ok res

(* [dest=]comp[;jump] *)
and parse_c_instr lexbuf first =
  let curr_loc = Lexer.curr_loc lexbuf in
  let next = Lexer.token lexbuf in
  let dest, comp_tok, comp_loc, next =
    match next with
    | Equals ->
      let curr' = Lexer.token lexbuf in
      let curr_loc' = Lexer.curr_loc lexbuf in
      let next' = Lexer.token lexbuf in
      Some first, curr', curr_loc', next'
    | _ -> None, first, curr_loc, next
  in
  let* comp =
    match comp_tok with
    | Symbol _ as comp -> Ok comp
    | _ ->
      Error
        (Lexer.mk_error_loc
           comp_loc
           ("Unexpected token: '" ^ Lexer.show_token comp_tok ^ "', expected comp"))
  in
  let jump, next =
    match next with
    | Semicolon ->
      let jump_tok = Lexer.token lexbuf in
      let next' = Lexer.token lexbuf in
      Some jump_tok, next'
    | _ -> None, next
  in
  let* () =
    match next with
    | Eol | Eof -> Ok ()
    | _ -> Error (Lexer.mk_error lexbuf "Expected end of line, got: '%s'")
  in
  Ok (T_instr { dest; comp; jump })
;;

let () =
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf [] in
  match res with
  | Ok ast -> print_endline (show_program ast)
  | Error err ->
    print_endline (Lexer.err_to_string err);
    exit 1
;;
