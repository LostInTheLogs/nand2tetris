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
  label_names_fn : string list ref;
  fn_names : string list ref;
}

let invalid_char char =
  match char with
  | '_' | '.' | '$' | ':' -> false
  | _ -> not (Char.is_alphanum char)

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

(* MEM LAYOUT 
0 SP, 1 LCL, 2 ARG, 3 THIS, 4 THAT,
5-12 temp segment
13-15 unused by programs *)

(** asm to push D onto the stack *)
let asm_push = [ "@SP"; "M=M+1"; "A=M-1"; "M=D" ]

let asm_pop dst = [ "@SP"; "AM=M-1"; "D=M" ] @ dst @ [ "M=D" ]
let ( @= ) dst src = [ src; "D=M"; dst; "M=D" ]

let mk_i_label { file; label_count_file } name =
  let count = !label_count_file in
  label_count_file := count + 1;
  file ^ "." ^ name ^ "." ^ Int.to_string count

let mk_static_lbl file idx = file ^ ".static." ^ idx

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

(** Takes a number from tokens, makes sure the rest is EOL *)
let get_last_number ((_, i) : local_span) tokens upper =
  let err = "Expected decimal between 0 and " ^ Int.to_string upper in
  match tokens with
  | { t = number_str; s } :: nil ->
      let open Result.Let_syntax in
      let offset_opt = Int.of_string_opt number_str in
      let%bind number = Result.of_option ~error:(LErr (err, s)) offset_opt in
      if
        String.for_all number_str ~f:Char.is_digit
        && 0 <= number && number <= upper
      then fin nil @@ Ok (number_str, number)
      else lerr err s
  | [] -> lerr "Unexpected EOL, expected a number" (i, i)

(** Takes a symbol from tokens *)
let get_symbol ((_, i) : local_span) tokens =
  match tokens with
  | { t = name; s } :: _ when Char.is_digit name.[0] ->
      lerr "Name can't start with a digit" s
  | { t = name; s } :: _ -> (
      let open Result.Let_syntax in
      let invalid = String.findi name ~f:(Fn.const invalid_char) in
      match invalid with
      | Some (i, _) -> lerr "Invalid char in name" (fst s + i, fst s + i)
      | _ -> Ok name)
  | [] -> lerr "Unexpected EOL, expected a name" (i, i)

(** Takes a unique symbol from tokens *)
let get_unique_symbol ((_, i) : local_span) tokens list =
  let open Result.Let_syntax in
  let%bind name = get_symbol (i, i) tokens in
  let[@warning "-8"] ({ s } :: rest) = tokens in
  let exists = List.mem !list name ~equal:equal_string in
  if exists then lerr "Redefined name" s else Ok name

(** Creates a stack operation with the offset from [token].
    @param upper Upper bound of the offset.
    @param post Asm after setting D to the offset.
    @param idx_f @offset transform. *)
let translate_stack_op s tokens upper post idx_f =
  let open Result.Let_syntax in
  let%bind num_str, _ = get_last_number s tokens upper in
  Ok ([ "@" ^ idx_f num_str; "D=A" ] @ post)

(** Creates a push operation with the offset from [token].
    @param upper Upper bound of the offset.
    @param getter Asm getter of the value: [D:offset -> D:value] 
    @param idx_f @offset transform. *)
let mk_push s tokens upper getter idx_f =
  let post = getter @ [ "@SP"; "M=M+1"; "A=M-1"; "M=D" ] in
  translate_stack_op s tokens upper post idx_f

let max_int = 32767

let translate_push { file } tokens =
  match tokens with
  | { s } :: { t = "constant" } :: tokens -> mk_push s tokens max_int [] Fn.id
  (* TODO: can do the 0/1 options manually for less asm*)
  | { s } :: { t = "pointer" } :: tokens ->
      mk_push s tokens 1 [ "@THIS"; "A=D+A"; "D=M" ] Fn.id
  | { s } :: { t = "this" } :: tokens ->
      mk_push s tokens max_int [ "@THIS"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | { s } :: { t = "that" } :: tokens ->
      mk_push s tokens max_int [ "@THAT"; "A=M"; "A=D+A"; "D=M" ] Fn.id
  | { s } :: { t = "local" } :: tokens ->
      mk_push s tokens max_int [ "@LCL"; "A=D+M"; "D=M" ] Fn.id
  | { s } :: { t = "argument" } :: tokens ->
      mk_push s tokens max_int [ "@ARG"; "A=D+M"; "D=M" ] Fn.id
  | { s } :: { t = "temp" } :: tokens ->
      mk_push s tokens 7 [ "@R5"; "A=D+A"; "D=M" ] Fn.id
  | { s } :: { t = "static" } :: tokens ->
      mk_push s tokens max_int [ "D=M" ] (mk_static_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> failwith "impossible"

(** Creates a pop operation with the offset from [token]. Uses [R13]
    @param upper Upper bound of the offset.
    @param getter Asm getter of the dst addr [D:offset -> D:dst_addr]
    @param idx_f @offset transform. *)
let mk_pop s tokens upper getter idx_f =
  (* R13 stores the dst addr *)
  let post =
    getter @ [ "@R13"; "M=D"; "@SP"; "AM=M-1"; "D=M"; "@R13"; "A=M"; "M=D" ]
  in
  translate_stack_op s tokens upper post idx_f

let translate_pop { file } tokens =
  match tokens with
  (* TODO: can do the 0/1 options manually for less asm*)
  | { s } :: { t = "pointer" } :: tokens ->
      mk_pop s tokens 1 [ "@THIS"; "D=D+A" ] Fn.id
  | { s } :: { t = "this" } :: tokens ->
      mk_pop s tokens max_int [ "@THIS"; "A=M"; "D=D+A" ] Fn.id
  | { s } :: { t = "that" } :: tokens ->
      mk_pop s tokens max_int [ "@THAT"; "A=M"; "D=D+A" ] Fn.id
  | { s } :: { t = "local" } :: tokens ->
      mk_pop s tokens max_int [ "@LCL"; "D=D+M" ] Fn.id
  | { s } :: { t = "argument" } :: tokens ->
      mk_pop s tokens max_int [ "@ARG"; "D=D+M" ] Fn.id
  | { s } :: { t = "temp" } :: tokens ->
      mk_pop s tokens 7 [ "@R5"; "D=D+A" ] Fn.id
  | { s } :: { t = "static" } :: tokens ->
      mk_pop s tokens max_int [] (mk_static_lbl file)
  | _ :: { s } :: _ -> lerr "Unknown segment" s
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> failwith "impossible"

let translate_goto { file; fn } tokens =
  match tokens with
  | { t = "goto"; s } :: (_ :: nil as rest) ->
      let open Result.Let_syntax in
      let%bind name = get_symbol s rest in
      fin nil @@ Ok [ "@" ^ file ^ "." ^ !fn ^ "." ^ name; "0;JMP" ]
  | { t = "if-goto"; s } :: (_ :: nil as rest) ->
      let open Result.Let_syntax in
      let%bind name = get_symbol s rest in
      fin nil
      @@ Ok
           [
             "@SP"; "A=M-1"; "D=M"; "@" ^ file ^ "." ^ !fn ^ "." ^ name; "D;JNE";
           ]
  | { s = _, b } :: [] -> lerr "Unexpected EOL" (b, b)
  | _ -> failwith "impossible"

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
      (* TODO: if 0 args we're overwriting the stack frame *)
      asm_pop [ "@R15" ] (* ret val *);
      (* R13 = ARG+1  (+1 because we add the return val on the stack )*)
      [ "@ARG"; "D=M"; "@R13"; "M=D+1" ];
      "@SP" @= "@LCL";
      asm_pop [ "@THAT" ];
      asm_pop [ "@THIS" ];
      asm_pop [ "@ARG" ];
      asm_pop [ "@LCL" ];
      asm_pop [ "@R14" ] (* ret addr *);
      "@SP" @= "@R13";
      [ "@R15"; "D=M"; "@SP"; "A=M-1"; "M=D" ] (* ret val *);
      [ "@R14"; "A=M"; "0;JMP" ] (* return *);
    ]

(* function fn 3:
  (function.`fn`)
  // push locals
  D=0
  (`3` times)
  asm_push
  ... (body) *)
let translate_function { fn; label_names_fn; fn_names } tokens =
  let[@warning "-8"] ({ s } :: rest) = tokens in
  let open Result.Let_syntax in
  let%bind name = get_unique_symbol s rest fn_names in
  let[@warning "-8"] ({ s } :: rest) = rest in
  let%bind _, n = get_last_number s rest max_int in
  label_names_fn := [];
  fn := name;
  Ok
    ([ "(function." ^ name ^ ")"; "D=0" ]
    @ List.concat (List.init n ~f:(Fn.const asm_push)))

let mk_call name n ret_lbl =
  List.concat
    [
      [ "@" ^ ret_lbl; "D=A" ];
      asm_push;
      [ "@LCL"; "D=M" ];
      asm_push;
      [ "@ARG"; "D=M" ];
      asm_push;
      [ "@THIS"; "D=M" ];
      asm_push;
      [ "@THAT"; "D=M" ];
      asm_push;
      "@LCL" @= "@SP";
      [ "@" ^ Int.to_string (n + 5); "D=A"; "@SP"; "D=M-D"; "@ARG"; "M=D" ];
      [ "@function." ^ name; "0;JMP"; "(" ^ ret_lbl ^ ")" ];
    ]
  @
  if n > 0 then [ "D=0" ]
  else [] @ List.concat (List.init n ~f:(Fn.const asm_push))

(*
Stack:
[  args ] [    stack frame    ] [ locals]
ARG1 ARG2 RET LCL ARG THIS THAT LCL1 LCL2
^                               ^
ARG (also return SP)            LCL
*)
let translate_call state tokens =
  let[@warning "-8"] ({ s } :: rest) = tokens in
  let open Result.Let_syntax in
  let%bind name = get_symbol s rest in
  let[@warning "-8"] ({ s } :: rest) = rest in
  let%bind _, n = get_last_number s rest max_int in
  let ret_lbl = mk_i_label state "return_addr" in
  Ok (mk_call name n ret_lbl)

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

let translate_label { file; fn; label_names_fn } tokens =
  let[@warning "-8"] ({ s } :: rest) = tokens in
  let open Result.Let_syntax in
  let%bind name = get_unique_symbol s rest label_names_fn in
  label_names_fn := name :: !label_names_fn;
  let[@warning "-8"] (_ :: nil) = rest in
  fin nil @@ Ok [ "(" ^ file ^ "." ^ !fn ^ "." ^ name ^ ")" ]

let translate_line state line =
  match tokenize_line line with
  | { t = "push" } :: _ as tokens -> translate_push state tokens
  | { t = "pop" } :: _ as tokens -> translate_pop state tokens
  | { t = "goto" | "if-goto" } :: _ as tokens -> translate_goto state tokens
  | { t = "function" } :: _ as tokens -> translate_function state tokens
  | { t = "call" } :: _ as tokens -> translate_call state tokens
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
          (acc
          @ ("/// " ^ state.file ^ ":" ^ Int.to_string (ln + 1) ^ " " ^ line)
            :: res)
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

let translate_file state name =
  (* let res = In_channel.with_open_text name @@ fun input -> tokenize input in *)
  (* TODO: sanitize file name with invalid_chars *)
  let state =
    {
      file = name;
      fn = ref "__toplevel";
      label_count_file = ref 0;
      label_names_fn = ref [];
      fn_names = state.fn_names;
    }
  in
  let res = translate stdin state in
  match res with
  | Ok ast -> Ok (String.concat_lines ast)
  | Error err -> Error (format_err name err)

(* let assembled = second_pass ast symbol_tbl |> List.map assemble_instr in
      let out_name = Filename.remove_extension name |> fun n -> n ^ ".TODO" in *)
(* Out_channel.with_open_text out_name @@ fun output -> *)
(* Out_channel.output_string output (String.concat "\n" assembled) *)
let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then prerr_endline "Specify input file(s)!"
  else
    let state =
      {
        file = "";
        fn = ref "__toplevel";
        label_count_file = ref 0;
        label_names_fn = ref [];
        fn_names = ref [];
      }
    in

    let res =
      argv |> Array.to_sequence_mutable
      |> Fn.flip Sequence.drop_eagerly 1
      |> Sequence.map ~f:(translate_file state)
      |> Sequence.to_list |> Result.all
    in
    match res with
    | Ok parts ->
        let init_asm =
          String.concat_lines
          @@ List.concat
               [
                 [ "@255"; "D=A"; "@SP"; "M=D" ];
                 (* [ "@function.Sys.init"; "0;JMP" ]; *)
                 mk_call "Sys.init" 0 "__END";
                 [ "0;JMP" ];
                 asm___return;
               ]
        in
        List.iter (init_asm :: parts) ~f:(fun part ->
            Out_channel.output_string stdout part;
            Out_channel.output_string stdout "\n")
    | Error err -> prerr_endline err
