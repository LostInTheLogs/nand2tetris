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

(*TODO: track current labels in fn. and functions. for redeclaration errors*)

type state = {
  file : string;
  fn : string ref;
  label_count_file : int ref;
  labels_fn : string list ref;
}
(** @param file Current file
    @param fn Current function
    @param label_count_file File local i label count
    @param labels_fn List of labels in the current function*)

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

(* STACK FRAME 

args (new ARG points here, return SP points here):
ARG1 ARG2
return addr:
RET
old pointers:
LCL ARG THIS THAT
locals: (new LCL points here)
LOC1 LOC2


push constant 0 (arg 0)
push constant 1 (arg 1)
call fn 2 :
  @ret_addr_rnd_lbl
  D=A
  // {asm_push 
  @SP
  M=M+1
  A=M-1
  M=D
  // asm_push}
  @LCL
  D=M
  asm_push
  @ARG
  D=M
  asm_push
  @THIS
  D=M
  asm_push
  @THAT
  D=M
  asm_push

  // LCL = SP
  @SP 
  D=M
  @LCL
  M=D

  // ARG = SP - (`2` + 5)
  @`2` `+5`
  D=A
  @SP
  D=M-D
  @ARG
  M=D

  @fn
  0;JMP

  (ret_addr_rnd_lbl)

function fn 3:

  // push locals
  D=0
  (`3` times)
  asm_push

  ... (body)

return:

  @__return
  0;JMP

(__return):

  // args (new ARG points here, return SP points here):
  // ARG1 ARG2
  // return addr:
  // RET
  // old pointers:
  // LCL ARG THIS THAT
  // locals: (new LCL points here)
  // LOC1 LOC2

  // ret value *ARG = stack_top
  @SP
  A=M-1
  D=M
  @ARG
  A=M
  M=D

  // SP = LCL
  @LCL
  D=M
  @SP
  M=D

  // pop -> that
  //@SP done above
  AM=M-1
  D=M
  @THAT
  M=D

  // pop -> this
  pop "@THIS"

  // R13 = arg
  @ARG
  D=M
  @R13
  M=D

  // pop -> arg
  pop "@ARG"

  // pop -> lcl
  pop "@LCL"

  // pop -> R14 ret addr
  pop "@R14"

  //SP=R13
  @R13
  D=M
  @SP
  M=D

  // ret
  @R14
  A=M
  0;JMP
 *)

(* MEM LAYOUT 
0 SP, 1 LCL, 2 ARG, 3 THIS, 4 THAT,
5-12 temp segment
13-15 unused by programs *)

(** asm to push D onto the stack *)
let asm_push = [ "@SP"; "M=M+1"; "A=M-1"; "M=D" ]

let asm_pop dst = [ "@SP"; "AM=M-1"; "D=M" ] @ dst @ [ "M=D" ]
let ( @= ) dst src = [ src; "D=M"; dst; "M=D" ]

(*
Stack:
[  args ] [    stack frame    ] [ locals]
ARG1 ARG2 RET LCL ARG THIS THAT LCL1 LCL2
^                               ^
ARG (also return SP)            LCL
*)
let asm___return =
  List.concat
    [
      [ "(__return)" ];
      asm_pop [ "@ARG"; "A=M" ] (* pop -> *ARG *);
      "@R13" @= "@ARG";
      "@SP" @= "@LCL";
      asm_pop [ "@THAT" ];
      asm_pop [ "@THIS" ];
      asm_pop [ "@ARG" ];
      asm_pop [ "@LCL" ];
      asm_pop [ "@R14" ] (* ret addr *);
      "@SP" @= "@R13";
      [ "@R14"; "A=M"; "0;JMP" ] (* return *);
    ]

let mk_function name lcl_count =
  [ "(" ^ name ^ ")"; "D=0" ]
  @ List.concat (List.init lcl_count ~f:(Fn.const asm_push))

let mk_i_label { file; label_count_file } name =
  let count = !label_count_file in
  label_count_file := count + 1;
  file ^ "." ^ name ^ "." ^ Int.to_string count

let mk_stack_lbl file idx = file ^ ".local." ^ idx

(** comp can't contain M or A*)
let mk_if state ~comp ~jump ~t ~f =
  let label = mk_i_label state "if" in
  let t_label = label ^ ".true" in
  let e_label = label ^ ".endif" in
  (*TODO: false comment label*)
  [ "// if"; "@" ^ t_label; comp ^ ";" ^ jump; "// (PROG.if.0.false)" ]
  @ f
  @ [ "@" ^ e_label; "0;JMP"; "(" ^ t_label ^ ")" ]
  @ t
  @ [ "(" ^ e_label ^ ")" ]

let mk_jumpover state ~jump ~over =
  let label = mk_i_label state "jumpover" in
  [ "@" ^ label; "D;" ^ jump ] @ over @ [ "(" ^ label ^ ")" ]

(** Creates a stack operation with the offset from [token].

    @param token The offset token.
    @param nil The next token.
    @param upper Upper bound of the offset.
    @param post Asm after setting D to the offset.
    @param idx_f @offset transform. *)
let translate_stack_op { t = index; s } nil upper post idx_f =
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
let translate_push token nil upper getter idx_f =
  let post = getter @ [ "@SP"; "M=M+1"; "A=M-1"; "M=D" ] in
  translate_stack_op token nil upper post idx_f

let max_int = 32767

let translate_push { file } tokens =
  match tokens with
  | _ :: { t = "constant" } :: tok :: nil ->
      translate_push tok nil max_int [] Fn.id
  (* TODO: can do the 0/1 options manually for less asm*)
  | _ :: { t = "pointer" } :: tok :: nil ->
      translate_push tok nil 1 [ "@THIS"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "this" } :: tok :: nil ->
      translate_push tok nil max_int [ "@THIS"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "that" } :: tok :: nil ->
      translate_push tok nil max_int [ "@THAT"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "local" } :: tok :: nil ->
      translate_push tok nil max_int [ "@LCL"; "A=D+M"; "D=M" ] Fn.id
  | _ :: { t = "argument" } :: tok :: nil ->
      translate_push tok nil max_int [ "@ARG"; "A=D+M"; "D=M" ] Fn.id
  | _ :: { t = "temp" } :: tok :: nil ->
      translate_push tok nil 7 [ "@R5"; "A=D+A"; "D=M" ] Fn.id
  | _ :: { t = "static" } :: tok :: nil ->
      translate_push tok nil max_int [ "D=M" ] (mk_stack_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

(** Creates a pop operation with the offset from [token]. Uses [R13]

    @param token The offset token.
    @param nil The next token.
    @param upper Upper bound of the offset.
    @param getter Asm getter of the dst addr [D:offset -> D:dst_addr]
    @param idx_f @offset transform. *)
let translate_pop token nil upper getter idx_f =
  (* R13 stores the dst addr *)
  let post =
    getter @ [ "@R13"; "M=D"; "@SP"; "AM=M-1"; "D=M"; "@R13"; "A=M"; "M=D" ]
  in
  translate_stack_op token nil upper post idx_f

let translate_pop { file } tokens =
  match tokens with
  (* TODO: can do the 0/1 options manually for less asm*)
  | _ :: { t = "pointer" } :: tok :: nil ->
      translate_pop tok nil 1 [ "@THIS"; "D=D+A" ] Fn.id
  | _ :: { t = "this" } :: tok :: nil ->
      translate_pop tok nil max_int [ "@THIS"; "A=M"; "D=D+A" ] Fn.id
  | _ :: { t = "that" } :: tok :: nil ->
      translate_pop tok nil max_int [ "@THAT"; "A=M"; "D=D+A" ] Fn.id
  | _ :: { t = "local" } :: tok :: nil ->
      translate_pop tok nil max_int [ "@LCL"; "D=D+M" ] Fn.id
  | _ :: { t = "argument" } :: tok :: nil ->
      translate_pop tok nil max_int [ "@ARG"; "D=D+M" ] Fn.id
  | _ :: { t = "temp" } :: tok :: nil ->
      translate_pop tok nil 7 [ "@R5"; "D=D+A" ] Fn.id
  | _ :: { t = "static" } :: tok :: nil ->
      translate_pop tok nil max_int [] (mk_stack_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

let translate_goto { file; fn } tokens =
  match tokens with
  | { t = "goto" } :: { t = label } :: nil ->
      Ok [ "@" ^ file ^ "." ^ !fn ^ "." ^ label; "0;JMP" ]
  | { t = "if-goto" } :: { t = label } :: nil ->
      Ok
        [ "@SP"; "A=M-1"; "D=M"; "@" ^ file ^ "." ^ !fn ^ "." ^ label; "D;JNE" ]
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

let translate_function { file; fn } tokens =
  match tokens with
  (* | _ :: { t = name } :: { t = label } :: nil ->
      Ok [ "@" ^ file ^ "." ^ !fn ^ "." ^ label; "0;JMP" ] *)
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> assert false

(* single arg *)

let translate_neg = Ok [ "@SP"; "A=M-1"; "M=-M" ]
let translate_not = Ok [ "@SP"; "A=M-1"; "M=!M" ]

(* double arg *)

let translate_add = Ok [ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D+M" ]
let translate_sub = Ok [ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=M-D" ]
let translate_and = Ok [ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D&M" ]
let translate_or = Ok [ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D|M" ]
let translate_return = Ok [ "@__return"; "0;JMP" ]

let translate_eq state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "D=D-M"; "M=0" ]
    @ mk_jumpover state ~jump:"JNE" ~over:[ "@SP"; "A=M-1"; "M=-1" ])

let translate_lt state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "D=M-D"; "M=-1" ]
    @ mk_jumpover state ~jump:"JLT" ~over:[ "@SP"; "A=M-1"; "M=0" ])

let translate_gt state =
  Ok
    ([ "@SP"; "AM=M-1"; "D=M"; "A=A-1"; "D=M-D"; "M=-1" ]
    @ mk_jumpover state ~jump:"JGT" ~over:[ "@SP"; "A=M-1"; "M=0" ])

let invalid_char char =
  match char with
  | '_' | '.' | '$' | ':' -> false
  | _ -> not (Char.is_alphanum char)

let translate_label { file; fn; labels_fn } tokens =
  match tokens with
  | _ :: { t = name; s } :: nil -> (
      let exists = List.mem !labels_fn name ~equal:equal_string in
      let invalid = String.findi name ~f:(Fn.const invalid_char) in
      fin nil
      @@
      match (exists, invalid) with
      | true, _ -> lerr "Redefined label" s
      | _, Some (i, _) -> lerr "Invalid char in label" (fst s + i, fst s + i)
      | _ ->
          labels_fn := name :: !labels_fn;
          Ok [ "(" ^ file ^ "." ^ !fn ^ "." ^ name ^ ")" ])
  | { s = _, i } :: _ -> lerr "Unexpected EOL" (i, i)
  | _ -> assert false

let translate_line state line =
  match tokenize_line line with
  | { t = "push" } :: _ as tokens -> translate_push state tokens
  | { t = "pop" } :: _ as tokens -> translate_pop state tokens
  | { t = "goto" | "if-goto" } :: _ as tokens -> translate_goto state tokens
  | { t = "function" } :: _ as tokens -> translate_function state tokens
  | { t = "neg" } :: nil -> fin nil @@ translate_neg
  | { t = "not" } :: nil -> fin nil @@ translate_not
  | { t = "add" } :: nil -> fin nil @@ translate_add
  | { t = "sub" } :: nil -> fin nil @@ translate_sub
  | { t = "and" } :: nil -> fin nil @@ translate_and
  | { t = "return" } :: nil -> fin nil @@ translate_return
  | { t = "or" } :: nil -> fin nil @@ translate_or
  | { t = "eq" } :: nil -> fin nil @@ translate_eq state
  | { t = "lt" } :: nil -> fin nil @@ translate_lt state
  | { t = "gt" } :: nil -> fin nil @@ translate_gt state
  | { t = "label" } :: _ as tokens -> translate_label state tokens
  | [] -> Ok []
  | { s } :: _ -> lerr "Unknown symbol" s

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
  (* TODO: sanitize file name with invalid_chars *)
  let state =
    {
      file = name;
      fn = ref "FN$TODO$";
      label_count_file = ref 0;
      labels_fn = ref [];
    }
  in
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
