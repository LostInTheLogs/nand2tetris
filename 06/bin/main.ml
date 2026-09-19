open Hackasm

type symbol_or_value = Literal of int | Symbol of string [@@deriving show]

type instruction =
  | A_instr of symbol_or_value
  | C_instr of { dest : string; comp : string; jump : string }
  | Label of string
[@@deriving show]

type program = instruction list [@@deriving show]

module ResultLet = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r
end

module OptionLet = struct
  let ( let* ) = Option.bind
  let ( let+ ) r f = Option.map f r
end

type span = string * int * int
(** line_str * beg * end (inclusive)*)

type error = string * span
type ln_error = string * int * span

let err_add_ln ln (msg, span) = (msg, ln, span)
let map_err_add_ln ln err = Result.map_error (err_add_ln ln) err

let parse_jump str =
  match str with
  | "JGT" -> Ok "001"
  | "JEQ" -> Ok "010"
  | "JGE" -> Ok "011"
  | "JLT" -> Ok "100"
  | "JNE" -> Ok "110"
  | "JLE" -> Ok "101"
  | "JMP" -> Ok "111"
  | _ ->
      Error "Invalid jump, expected one of: JGT, JEQ, JGE, JLT, JNE, JLE, JMP"

let parse_dest str =
  match str with
  | "A" -> Ok "100"
  | "M" -> Ok "001"
  | "D" -> Ok "010"
  | "AM" -> Ok "101"
  | "AD" -> Ok "110"
  | "MD" -> Ok "011"
  | "AMD" -> Ok "111"
  | _ -> Error "Invalid dest, expected one of: A, M, D, AM, AD, MD, AMD"

let parse_comp str =
  match str with
  (* a=0 *)
  | "0" -> Ok "0101010"
  | "1" -> Ok "0111111"
  | "-1" -> Ok "0111010"
  | "D" -> Ok "0001100"
  | "A" -> Ok "0110000"
  | "!D" -> Ok "0001101"
  | "!A" -> Ok "0110001"
  | "-D" -> Ok "0001111"
  | "-A" -> Ok "0110011"
  | "D+1" -> Ok "0011111"
  | "A+1" -> Ok "0110111"
  | "D-1" -> Ok "0001110"
  | "A-1" -> Ok "0110010"
  | "D+A" -> Ok "0000010"
  | "D-A" -> Ok "0010011"
  | "A-D" -> Ok "0000111"
  | "D&A" -> Ok "0000000"
  | "D|A" -> Ok "0010101"
  (* a=1 *)
  | "M" -> Ok "1110000"
  | "!M" -> Ok "1110001"
  | "-M" -> Ok "1110011"
  | "M+1" -> Ok "1110111"
  | "M-1" -> Ok "1110010"
  | "D+M" -> Ok "1000010"
  | "D-M" -> Ok "1010011"
  | "M-D" -> Ok "1000111"
  | "D&M" -> Ok "1000000"
  | "D|M" -> Ok "1010101"
  | _ -> Error "Invalid comp"

let invalid_char char =
  match char with
  | '_' | '.' | '$' | ':' -> false
  | _ -> not (Char.Ascii.is_alphanum char)

(* `@value` *)
let parse_a_instr line =
  let value = String.sub line 1 (String.length line - 1) in
  if String.length line < 2 then
    Error ("Expected a symbol name or number", (line, 1, 1))
  else if Char.Ascii.is_digit line.[1] then
    let not_digit char = not (Char.Ascii.is_digit char) in
    let invalid_idx = String.find_first_index not_digit ~start:1 line in
    match (invalid_idx, int_of_string_opt value) with
    | None, Some num when 0 <= num && num <= 32767 -> Ok (A_instr (Literal num))
    | None, _ ->
        Error ("Expected 0 <= n <= 32767", (line, 1, String.length line - 1))
    | Some i, _ -> Error ("Expected a digit", (line, i, i))
  else
    match String.find_first_index invalid_char ~start:1 line with
    | None -> Ok (A_instr (Symbol value))
    | Some i -> Error ("Unexpected character in symbol name", (line, i, i))

let parse_label line =
  let len = String.length line in
  match String.find_first_index invalid_char ~start:1 line with
  | None -> Error ("Expected ')'", (line, len, len))
  | Some i when line.[i] = ')' ->
      if i = 1 then Error ("Expected label", (line, 1, 1))
      else if Char.Ascii.is_digit line.[1] then
        Error ("Labels can't start with a digit", (line, 1, 1))
      else if i = len - 1 then Ok (Label (String.sub line 1 (len - 2)))
      else Error ("Expected EOL", (line, len - 1, len - 1))
  | Some i -> Error ("Unexpected character in label", (line, i, i))

(* `[dest=]comp[;jump]` *)
let parse_c_instr line =
  let open ResultLet in
  let len = String.length line in
  let eq = String.index_opt line '=' in
  let sc = String.index_opt line ';' in
  let dest_res, comp_beg =
    match eq with
    | None -> (Ok "000", 0)
    | Some i ->
        let dest =
          parse_dest (String.sub line 0 i)
          |> Result.map_error (fun e -> (e, (line, 0, max 0 (i - 1))))
        in
        (dest, i + 1)
  in
  let* dest = dest_res in
  let jump_res, comp_end =
    match sc with
    | None -> (Ok "000", len)
    | Some i ->
        let jump =
          parse_jump (String.sub line (i + 1) (len - i - 1))
          |> Result.map_error (fun e ->
              (e, (line, i + 1, max (i + 1) (len - 1))))
        in
        (jump, i)
  in
  let* jump = jump_res in
  let comp_res =
    parse_comp (String.sub line comp_beg (comp_end - comp_beg))
    |> Result.map_error (fun e ->
        (e, (line, comp_beg, max comp_beg (comp_end - 1))))
  in
  let* comp = comp_res in
  Ok (C_instr { dest; comp; jump })

let parse_line line =
  match line.[0] with
  | '@' -> parse_a_instr line
  | '(' -> parse_label line
  | _ -> parse_c_instr line

let prepare_line line =
  let stripped = String.trim line in
  match String.index_opt stripped '/' with
  | Some i -> String.sub stripped 0 i
  | None -> stripped

let parse channel =
  let open ResultLet in
  let rec loop ln acc =
    match Option.map prepare_line (In_channel.input_line channel) with
    | Some "" -> loop (ln + 1) acc
    | Some line ->
        let* cmd = map_err_add_ln ln (parse_line line) in
        loop (ln + 1) (cmd :: acc)
    | None -> Ok (List.rev acc)
  in
  loop 0 []

let format_err file (msg, nr, (line, a, b)) =
  let carets =
    String.init
      (String.length line + 1)
      (fun i -> if a <= i && i <= b then '^' else ' ')
  in
  Printf.sprintf "%s:%d:%d\n" file (nr + 1) (a + 1)
  ^ line ^ "\n" ^ carets ^ "\nError: " ^ msg

let () =
  let res = parse stdin in
  match res with
  | Ok ast -> print_string (show_program ast)
  | Error err -> print_endline (format_err "stdin" err)
