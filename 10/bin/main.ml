open Jackc
open Base
open Stdio
open Poly

type span_context = { source : string; filename : string }
type span = int * int * span_context
type 'a spanned = { t : 'a; s : span }

let substr str a b = String.sub str ~pos:a ~len:(b - a)

let format_error (msg, (a, b, ctx)) =
  let { source; filename } = ctx in
  let rawlines = String.split ~on:'\n' source in
  let newline_locations =
    List.fold rawlines
      ~f:(fun acc x -> (List.hd_exn acc + String.length x + 1) :: acc)
      ~init:[ -1 ]
    |> List.rev |> List.tl_exn
  in
  let get_line_pos ln =
    if ln = 0 then 0 else List.nth_exn newline_locations (ln - 1) - 1
  in
  let get_line_char x =
    let ln = fst @@ List.findi_exn newline_locations ~f:(fun _ ln -> ln >= x) in
    let char = x - get_line_pos ln in
    (ln, char)
  in
  let lines =
    rawlines
    |> List.map ~f:(String.tr ~target:'\t' ~replacement:' ')
    |> List.map ~f:(String.tr ~target:'\r' ~replacement:' ')
  in
  let ln_a, char_a = get_line_char a in
  let ln_b, char_b = get_line_char b in
  let lines = List.sub lines ~pos:ln_a ~len:(ln_b - ln_a + 1) in
  let fmt_line i line =
    let line_nr = ln_a + i in
    let line_pos = get_line_pos line_nr in
    let carets =
      String.init
        (String.length line + 1)
        ~f:(fun char_i ->
          let j = line_pos + char_i in
          if (a <= j && j < b) || (j = a && a = b) then '^' else ' ')
    in
    Printf.sprintf "%4d | %s\n" (line_nr + 1) line ^ "       " ^ carets ^ "\n"
  in
  Printf.sprintf "%s:%d:%d: error %s\n" filename (ln_a + 1) (char_a + 1) msg
  ^ String.concat (List.mapi lines ~f:fmt_line)

let err msg (span : span) =
  Stdio.prerr_endline (format_error (msg, span));
  Stdlib.exit 1

type tok =
  | TSemicolon
  | TLParen of char
  | TRParen of char
  | TIdentifier of string
  | TString of string
[@@deriving sexp]

type tokens = tok list [@@deriving sexp]
type stok = tok spanned
type tokenize_mode = MNone | MString of int | MIdent of int

let tokenize str file =
  let ctx = { source = str; filename = file } in
  let len = String.length str in
  let substr a b = String.sub str ~pos:a ~len:(b - a) in
  let rec go (i, mode, acc) =
    let flush () =
      match mode with
      | MNone -> acc
      | MString beg -> err "unclosed str" (beg - 1, i - 1, ctx)
      | MIdent beg ->
          { t = TIdentifier (substr beg i); s = (beg, i, ctx) } :: acc
    in
    if i >= len then flush ()
    else
      let char = str.[i] in
      go
      @@
      match (mode, char) with
      | MString beg, '"' ->
          ( i + 1,
            MNone,
            { t = TString (substr beg i); s = (beg, i, ctx) } :: acc )
      | MString _, _ -> (i + 1, mode, acc)
      | _, (';' | '-' | '+' | '*' | '/' | '=' | '~') ->
          ( i + 1,
            MNone,
            { t = TIdentifier (Char.to_string char); s = (i, i + 1, ctx) }
            :: flush () )
      | _, ('(' | '[' | '{') ->
          (i + 1, MNone, { t = TLParen char; s = (i, i + 1, ctx) } :: flush ())
      | _, (')' | ']' | '}') ->
          (i + 1, MNone, { t = TRParen char; s = (i, i + 1, ctx) } :: flush ())
      | _, '"' -> (i + 1, MString (i + 1), flush ())
      | _, (' ' | '\n' | '\t' | '\r') -> (i + 1, MNone, flush ())
      | MIdent _, _ -> (i + 1, mode, acc)
      | MNone, _ -> (i + 1, MIdent i, acc)
  in
  List.rev @@ go (0, MNone, [])

let compile_file file =
  (* In_channel.with_open_text name @@ fun input -> parse input symbol_tbl *)
  let str = In_channel.input_all stdin in
  let tokens = tokenize str file in
  let toks = List.map tokens ~f:(fun { t } -> t) in
  Stdio.print_s (sexp_of_tokens toks)

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then Stdio.prerr_endline "Specify input file(s)!"
  else
    let res =
      argv |> Array.to_sequence_mutable
      |> Fn.flip Sequence.drop_eagerly 1
      |> Sequence.iter ~f:compile_file
      (* |> Sequence.to_list |> Result.all *)
    in
    res
