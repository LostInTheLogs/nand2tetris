open Base
open Stdio

type err_info = { line : string }
type local_span = int * int
type local_error = LErr of string * local_span

(* local_span * line_nr  * err_info *)
type span = local_span * int * err_info
type stoken = { t : string; s : local_span }
type error = GErr of string * span

(* let err_add_ln ln (msg, span) = (msg, ln, span) *)
(* let map_err_add_ln ln err = Result.map_error (err_add_ln ln) err *)

type state = { file : string; label_count : int ref }

let tokenize_line line =
  let stripped =
    String.strip
      (match String.substr_index line ~pattern:"//" with
      | Some i -> String.prefix line i
      | None -> line)
  in
  let len = String.length stripped in
  let rec find_start i =
    if i >= len then []
    else if Char.is_whitespace stripped.[i] then find_start (i + 1)
    else find_end i (i + 1)
  and find_end start curr =
    if curr >= len || Char.is_whitespace stripped.[curr] then
      let str = String.sub stripped ~pos:start ~len:(curr - start) in
      { t = str; s = (start, curr - 1) } :: find_start curr
    else find_end start (curr + 1)
  in
  find_start 0

let lerr a b = Error (LErr (a, b))

let translate_push tokens =
  match tokens with
  | [ _; { t = "constant" }; { t = const; s } ] ->
      let open Result.Let_syntax in
      let wrr = Int.of_string_opt const in
      let%bind v =
        Result.of_option ~error:(LErr ("Expected a number", s)) wrr
      in
      if String.for_all const ~f:Char.is_digit && 0 <= v && v <= 32767 then
        Ok [ "@" ^ const; "D=A"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1" ]
      else lerr "invalid offset, must be [0; 32767]" s
  | _ :: { s } :: _ -> lerr "Unknown symbol" s
  | { s = _, b } :: _ -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

let translate_add =
  Ok [ "@SP"; "A=M-1"; "D=M"; "@SP"; "M=M-1"; "A=M-1"; "M=D+M" ]

let mk_label { file; label_count } name =
  let count = !label_count in
  label_count := count + 1;
  file ^ "." ^ name ^ "." ^ Int.to_string count

(* comp can't contain M or A*)
let mk_if state ~comp ~jump ~t ~f =
  let label = mk_label state "if" in
  let t_label = label ^ ".true" in
  let e_label = label ^ ".endif" in
  [ "// if"; "@" ^ t_label; comp ^ ";" ^ jump; "// (PROG.if.0.false)" ]
  @ f
  @ [ "@" ^ e_label; "0;JMP"; "(" ^ t_label ^ ")" ]
  @ t
  @ [ "(" ^ e_label ^ ")" ]

let translate_eq state =
  Ok
    ([ "@SP"; "A=M-1"; "D=M"; "@SP"; "M=M-1"; "A=M-1"; "D=D-M" ]
    @ mk_if state ~comp:"D" ~jump:"JEQ" ~t:[ "D=-1" ] ~f:[ "D=0" ]
    @ [ "@SP"; "A=M-1"; "M=D" ])

let translate_line line state =
  match tokenize_line line with
  | { t = "push" } :: rest as tokens -> translate_push tokens
  | { t = "add" } :: [] -> translate_add
  | { t = "eq" } :: [] -> translate_eq state
  | _ -> lerr "Unknown symbol, or TODO: extra args" (0, 0)

let translate input state =
  let rec loop ln acc =
    let line_opt = In_channel.input_line input in
    match line_opt with
    | Some "" -> loop (ln + 1) acc
    | Some line ->
        let open Result.Let_syntax in
        let mk_global (LErr (msg, span)) = GErr (msg, (span, ln, { line })) in
        let map_global a = Result.map_error ~f:mk_global a in
        let%bind res = map_global (translate_line line state) in
        loop (ln + 1)
          (acc @ (("/// " ^ Int.to_string (ln + 1) ^ " " ^ line) :: res))
    | None -> Ok acc
  in
  loop 0 []

let format_err file (GErr (msg, span)) =
  let (a, b), ln, { line } = span in
  let carets =
    String.init
      (String.length line + 1)
      ~f:(fun i -> if a <= i && i <= b then '^' else ' ')
  in

  Printf.sprintf "%s:%d:%d\n" file (ln + 1) (a + 1)
  ^ line ^ "\n" ^ carets ^ "\nError: " ^ msg

let translate_file name =
  (* let res = In_channel.with_open_text name @@ fun input -> tokenize input in *)
  let state = { file = name; label_count = ref 0 } in
  let res = translate stdin state in
  match res with
  | Ok ast ->
      (* let assembled = second_pass ast symbol_tbl |> List.map assemble_instr in
      let out_name = Filename.remove_extension name |> fun n -> n ^ ".TODO" in *)
      (* Out_channel.with_open_text out_name @@ fun output -> *)
      (* Out_channel.output_string output (String.concat "\n" assembled) *)
      print_endline @@ String.concat_lines ast
  | Error err -> print_endline (format_err name err)

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then print_endline "Specify input file(s)!"
  else
    argv |> Array.to_sequence_mutable
    |> Fn.flip Sequence.drop_eagerly 1
    |> Sequence.iter ~f:translate_file

(* let invalid_char char =
  match char with
  | '_' | '.' | '$' | ':' -> false
  | _ -> not (Char.is_alphanum char) *)
