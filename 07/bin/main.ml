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

let fin arr res =
  match arr with [] -> res | { s } :: _ -> lerr "Expected EOL" s

(* MEM LAYOUT 
0 SP, 1 LCL, 2 ARG, 3 THIS, 4 THAT,
5-12 temp segment
13-15 unused by programs *)

let mk_label { file; label_count } name =
  let count = !label_count in
  label_count := count + 1;
  file ^ "." ^ name ^ "." ^ Int.to_string count

(** comp can't contain M or A*)
let mk_if state ~comp ~jump ~t ~f =
  let label = mk_label state "if" in
  let t_label = label ^ ".true" in
  let e_label = label ^ ".endif" in
  [ "// if"; "@" ^ t_label; comp ^ ";" ^ jump; "// (PROG.if.0.false)" ]
  @ f
  @ [ "@" ^ e_label; "0;JMP"; "(" ^ t_label ^ ")" ]
  @ t
  @ [ "(" ^ e_label ^ ")" ]

let mk_stack_lbl file idx = file ^ ".local." ^ idx

(** Creates a stack operation with the offset from [token].

    @param token The offset token.
    @param nil The next token.
    @param upper Upper bound of the offset.
    @param post Asm after setting D to the offset.
    @param idx_f @offset transform. *)
let mk_stack_op { t = index; s } nil upper post idx_f =
  fin nil
  @@
  let open Result.Let_syntax in
  let offset_opt = Int.of_string_opt index in
  let%bind offset =
    Result.of_option ~error:(LErr ("Expected a number", s)) offset_opt
  in
  if String.for_all index ~f:Char.is_digit && 0 <= offset && offset <= upper
  then Ok ([ "@" ^ idx_f index; "D=A" ] @ post)
  else lerr ("invalid offset, must be between 0 and " ^ Int.to_string upper) s

(** Creates a push operation with the offset from [token].

    @param token The offset token.
    @param nil The next token.
    @param upper Upper bound of the offset.
    @param getter Asm getter of the value: [D:offset -> D:value] 
    @param idx_f @offset transform. *)
let mk_push token nil upper getter idx_f =
  let post = getter @ [ "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1" ] in
  mk_stack_op token nil upper post idx_f

let translate_push { file } tokens =
  match tokens with
  | _ :: { t = "constant" } :: tok :: nil -> mk_push tok nil 32767 [] Fn.id
  (* TODO: can do the 0/1 options manually for less asm*)
  | _ :: { t = "pointer" } :: tok :: nil ->
      mk_push tok nil 1 [ "@THIS"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "this" } :: tok :: nil ->
      mk_push tok nil 32767 [ "@THIS"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "that" } :: tok :: nil ->
      mk_push tok nil 32767 [ "@THAT"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "local" } :: tok :: nil ->
      mk_push tok nil 32767 [ "@LCL"; "A=D+M"; "D=M" ] Fn.id
  | _ :: { t = "argument" } :: tok :: nil ->
      mk_push tok nil 32767 [ "@ARG"; "A=D+M"; "D=M" ] Fn.id
  | _ :: { t = "temp" } :: tok :: nil ->
      mk_push tok nil 7 [ "@R5"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "static" } :: tok :: nil ->
      mk_push tok nil 32767 [ "D=M" ] (mk_stack_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: _ -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

(** Creates a pop operation with the offset from [token].

    @param token The offset token.
    @param nil The next token.
    @param upper Upper bound of the offset.
    @param getter Asm getter of the dst addr [D:offset -> D:dst_addr]
    @param idx_f @offset transform. *)
let mk_pop token nil upper getter idx_f =
  (* R13 stores the dst addr *)
  let post =
    getter @ [ "@R13"; "M=D"; "@SP"; "AM=M-1"; "D=M"; "@R13"; "A=M"; "M=D" ]
  in
  mk_stack_op token nil upper post idx_f

let translate_pop { file } tokens =
  match tokens with
  (* TODO: can do the 0/1 options manually for less asm*)
  | _ :: { t = "pointer" } :: tok :: nil ->
      mk_pop tok nil 1 [ "@THIS"; "D=D+A" ] Fn.id
  | _ :: { t = "this" } :: tok :: nil ->
      mk_pop tok nil 32767 [ "@THIS"; "A=M"; "D=D+A" ] Fn.id
  | _ :: { t = "that" } :: tok :: nil ->
      mk_pop tok nil 32767 [ "@THAT"; "A=M"; "D=D+A" ] Fn.id
  | _ :: { t = "local" } :: tok :: nil ->
      mk_pop tok nil 32767 [ "@LCL"; "D=D+M" ] Fn.id
  | _ :: { t = "argument" } :: tok :: nil ->
      mk_pop tok nil 32767 [ "@ARG"; "D=D+M" ] Fn.id
  | _ :: { t = "temp" } :: tok :: nil ->
      mk_pop tok nil 7 [ "@R5"; "D=D+A" ] Fn.id
  | _ :: { t = "static" } :: tok :: nil ->
      mk_pop tok nil 32767 [] (mk_stack_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: _ -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

(* single arg *)

let translate_neg = Ok [ "@SP"; "A=M-1"; "M=-M" ]
let translate_not = Ok [ "@SP"; "A=M-1"; "M=!M" ]

(* double arg *)

let translate_add = Ok [ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "M=D+M" ]
let translate_sub = Ok [ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "M=M-D" ]
let translate_and = Ok [ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "M=D&M" ]
let translate_or = Ok [ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "M=D|M" ]

let translate_eq state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "D=D-M" ]
    @ mk_if state ~comp:"D" ~jump:"JEQ" ~t:[ "D=-1" ] ~f:[ "D=0" ]
    @ [ "@SP"; "A=M-1"; "M=D" ])

let translate_lt state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "D=D-M" ]
    @ mk_if state ~comp:"D" ~jump:"JGT" ~t:[ "D=-1" ] ~f:[ "D=0" ]
    @ [ "@SP"; "A=M-1"; "M=D" ])

let translate_gt state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "@SP"; "A=M-1"; "D=D-M" ]
    @ mk_if state ~comp:"D" ~jump:"JLT" ~t:[ "D=-1" ] ~f:[ "D=0" ]
    @ [ "@SP"; "A=M-1"; "M=D" ])

let translate_line state line =
  match tokenize_line line with
  | { t = "push" } :: rest as tokens -> translate_push state tokens
  | { t = "pop" } :: rest as tokens -> translate_pop state tokens
  | { t = "neg" } :: nil -> fin nil @@ translate_neg
  | { t = "not" } :: nil -> fin nil @@ translate_not
  | { t = "add" } :: nil -> fin nil @@ translate_add
  | { t = "sub" } :: nil -> fin nil @@ translate_sub
  | { t = "and" } :: nil -> fin nil @@ translate_and
  | { t = "or" } :: nil -> fin nil @@ translate_or
  | { t = "eq" } :: nil -> fin nil @@ translate_eq state
  | { t = "lt" } :: nil -> fin nil @@ translate_lt state
  | { t = "gt" } :: nil -> fin nil @@ translate_gt state
  | [] -> Ok []
  | _ -> lerr "Unknown symbol, or extra args" (0, 0)

let translate input state =
  let rec loop ln acc =
    let line_opt = In_channel.input_line input in
    match line_opt with
    | Some "" -> loop (ln + 1) acc
    | Some line ->
        let open Result.Let_syntax in
        let mk_global (LErr (msg, span)) = GErr (msg, (span, ln, { line })) in
        let map_global a = Result.map_error ~f:mk_global a in
        let%bind res = map_global (translate_line state line) in
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
  | Error err -> prerr_endline (format_err name err)

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then prerr_endline "Specify input file(s)!"
  else
    argv |> Array.to_sequence_mutable
    |> Fn.flip Sequence.drop_eagerly 1
    |> Sequence.iter ~f:translate_file

(* let invalid_char char =
  match char with
  | '_' | '.' | '$' | ':' -> false
  | _ -> not (Char.is_alphanum char) *)
