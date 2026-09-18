open Hackasm
open Base
open Stdio

type dest =
  | M
  | D
  | MD
  | A
  | AM
  | AD
  | AMD
[@@deriving sexp]

type comp =
  | Zero
  | One
  | D_op
  | A_op
[@@deriving sexp]

type jump =
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
[@@deriving sexp]

type symbol_or_value =
  | Literal of int
  | Symbol of string
[@@deriving sexp]

type instruction =
  | A_instr of symbol_or_value
  | C_instr of
      { dest : dest option
      ; comp : comp
      ; jump : jump option
      }
  | Label of string
[@@deriving sexp]

type program = instruction list [@@deriving sexp]

let rec parse lexbuf acc =
  let open Result.Let_syntax in
  let token = Lexer.token lexbuf in
  match token with
  | Eol -> parse lexbuf acc
  | Eof -> Ok (List.rev acc)
  | At ->
    let%bind instr = parse_a_instr lexbuf in
    parse lexbuf (instr :: acc)
  | _ -> Error (Lexer.mk_error_lexeme lexbuf "Unexpected token: '%s'")

and expect_eol_or_eof lexbuf =
  match Lexer.token lexbuf with
  | Eol | Eof -> Ok ()
  | _ -> Error (Lexer.mk_error_lexeme lexbuf "Expected end of line, got: '%s'")

and parse_a_instr lexbuf =
  let open Result.Let_syntax in
  let%bind res =
    match Lexer.token lexbuf with
    | Number num -> Ok (A_instr (Literal num))
    | Symbol sym -> Ok (A_instr (Symbol sym))
    | _ -> Error (Lexer.mk_error_lexeme lexbuf "Unexpected token: '%s'")
  in
  let%map () = expect_eol_or_eof lexbuf in
  res
;;

let () =
  let lexbuf = Lexing.from_channel Stdio.stdin in
  let res = parse lexbuf [] in
  match res with
  | Ok ast -> Stdio.print_s (sexp_of_program ast)
  | Error err ->
    Stdio.print_endline (Lexer.err_to_string err);
    Stdlib.exit 1
;;
