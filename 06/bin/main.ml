open Hackasm

type symbol_or_value = Literal of int | Symbol of string [@@deriving show]

type instruction =
  | A_instr of symbol_or_value
  | C_instr of { dest : string; comp : string; jump : string }
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
  | "JNE" -> Ok "101"
  | "JLE" -> Ok "110"
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

let parse_label line pc symbol_tbl =
  let len = String.length line in
  match String.find_first_index invalid_char ~start:1 line with
  | None -> Error ("Expected ')'", (line, len, len))
  | Some i when line.[i] = ')' ->
      if i = 1 then Error ("Expected label", (line, 1, 1))
      else if Char.Ascii.is_digit line.[1] then
        Error ("Labels can't start with a digit", (line, 1, 1))
      else if i = len - 1 then
        let str = String.sub line 1 (len - 2) in
        if Hashtbl.mem symbol_tbl str then
          Error ("Expected EOL", (line, len - 1, len - 1))
        else Ok (Hashtbl.add symbol_tbl str pc)
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

let prepare_line line =
  let stripped = String.trim line in
  match String.index_opt stripped '/' with
  | Some i -> String.sub stripped 0 i
  | None -> stripped

let parse channel symbol_tbl =
  let open ResultLet in
  let rec loop ln pc acc =
    match Option.map prepare_line (In_channel.input_line channel) with
    | Some "" -> loop (ln + 1) pc acc
    | Some line when line.[0] = '(' ->
        let unit_opt = parse_label line pc symbol_tbl in
        let* () = map_err_add_ln ln unit_opt in
        loop (ln + 1) pc acc
    | Some line ->
        let cmd_opt =
          if line.[0] = '@' then parse_a_instr line else parse_c_instr line
        in
        let* cmd = map_err_add_ln ln cmd_opt in
        loop (ln + 1) (pc + 1) (cmd :: acc)
    | None -> Ok (List.rev acc)
  in
  loop 0 0 []

let format_err file (msg, nr, (line, a, b)) =
  let carets =
    String.init
      (String.length line + 1)
      (fun i -> if a <= i && i <= b then '^' else ' ')
  in
  Printf.sprintf "%s:%d:%d\n" file (nr + 1) (a + 1)
  ^ line ^ "\n" ^ carets ^ "\nError: " ^ msg

let builtin_symbols =
  [
    ("SP", 0);
    ("LCL", 1);
    ("ARG", 2);
    ("THIS", 3);
    ("THAT", 4);
    ("R0", 0);
    ("R1", 1);
    ("R2", 2);
    ("R3", 3);
    ("R4", 4);
    ("R5", 5);
    ("R6", 6);
    ("R7", 7);
    ("R8", 8);
    ("R9", 9);
    ("R10", 10);
    ("R11", 11);
    ("R12", 12);
    ("R13", 13);
    ("R14", 14);
    ("R15", 15);
    ("SCREEN", 16384);
    ("KBD", 24576);
  ]

let int_to_binary16 n =
  String.init 16 (fun i -> if (n lsr (15 - i)) land 1 = 1 then '1' else '0')

let assemble_instr instr =
  match instr with
  | A_instr (Literal addr) -> int_to_binary16 addr
  | C_instr { dest; comp; jump } -> "111" ^ comp ^ dest ^ jump
  | _ -> assert false

let second_pass program symbol_tbl =
  let rec loop prg var_i acc =
    match prg with
    | A_instr (Symbol smb) :: rest -> (
        match Hashtbl.find_opt symbol_tbl smb with
        | Some v -> loop rest var_i (A_instr (Literal v) :: acc)
        | None ->
            let v = 16 + var_i in
            Hashtbl.add symbol_tbl smb v;
            loop rest (var_i + 1) (A_instr (Literal v) :: acc))
    | a :: rest -> loop rest var_i (a :: acc)
    | [] -> List.rev acc
  in
  loop program 0 []

let assemble_file name =
  let symbol_tbl = Hashtbl.of_seq (List.to_seq builtin_symbols) in
  let res =
    In_channel.with_open_text name @@ fun input -> parse input symbol_tbl
  in
  match res with
  | Ok ast ->
      let assembled = second_pass ast symbol_tbl |> List.map assemble_instr in
      let out_name = Filename.remove_extension name |> fun n -> n ^ ".hack" in
      Out_channel.with_open_text out_name @@ fun output ->
      Out_channel.output_string output (String.concat "\n" assembled)
  | Error err -> print_endline (format_err name err)

let () =
  if Array.length Sys.argv < 2 then print_endline "Specify input file(s)!"
  else Array.to_seq Sys.argv |> Seq.drop 1 |> Seq.iter assemble_file
