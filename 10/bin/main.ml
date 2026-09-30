open Jackc
open Base
open Stdio
open Poly

type span_context = { source : string; filename : string }
type span = int * int * span_context
type 'a spanned = { v : 'a; s : span }

let sexp_of_spanned sexp_of_t { v } = sexp_of_t v
(**)

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
  Printf.sprintf "%s:%d:%d: error: %s\n" filename (ln_a + 1) (char_a + 1) msg
  ^ String.concat (List.mapi lines ~f:fmt_line)

let err msg (span : span) =
  Stdio.prerr_endline (format_error (msg, span));
  Stdlib.exit 1

type tok =
  | TLParen of char
  | TRParen of char
  | TIdentifier of string
  | TKeyword of string
  | TNumber of int
  | TString of string
[@@deriving sexp_of]

type tokens = tok list [@@deriving sexp_of]
type stok = tok spanned [@@deriving sexp_of]

type tokenize_mode =
  | MNone
  | MString of int
  | MIdent of int
  | MLineComment of int
  | MComment of int * int (* beg, level *)

let tokenize_ident str beg i ctx =
  match str with
  | _ when String.for_all str ~f:(fun c -> '0' <= c && c <= '9') -> (
      match Int.of_string_opt str with
      | Some n -> TNumber n
      | None -> err "invalid number" (beg, i, ctx))
  | "class" | "field" | "constructor" | "method" | "return" | "do" ->
      TKeyword str
  | _ -> TIdentifier str

let tokenize str file =
  let ctx = { source = str; filename = file } in
  let len = String.length str in
  let substr a b = String.sub str ~pos:a ~len:(b - a) in
  let rec go (i, mode, acc) =
    let flush () =
      match mode with
      | MNone | MLineComment _ -> acc
      | MString beg -> err "unclosed str" (beg, i - 1, ctx)
      | MComment (beg, _) -> err "unclosed comment" (beg, i - 1, ctx)
      | MIdent beg ->
          { v = tokenize_ident (substr beg i) beg i ctx; s = (beg, i, ctx) }
          :: acc
    in
    if i >= len then flush ()
    else
      let char = str.[i] in
      let prev = if i > 0 then str.[i - 1] else ' ' in
      let next = if i + 1 < len then str.[i + 1] else ' ' in
      go
      @@
      match (mode, char) with
      | MString beg, '"' ->
          ( i + 1,
            MNone,
            { v = TString (substr (beg + 1) i); s = (beg, i + 1, ctx) } :: acc
          )
      | MString _, _ -> (i + 1, mode, acc)
      | MLineComment beg, '\n' -> (i + 1, MNone, acc)
      | MLineComment _, _ -> (i + 1, mode, acc)
      | MComment (beg, lvl), '/' when next = '*' ->
          (i + 2, MComment (beg, lvl + 1), acc)
      | MComment (beg, lvl), '/' when prev = '*' ->
          (i + 1, (if lvl = 0 then MNone else MComment (beg, lvl - 1)), acc)
      | MComment _, _ -> (i + 1, mode, acc)
      | _, '/' when next = '*' -> (i + 2, MComment (i, 0), acc)
      | _, '/' when next = '/' -> (i + 2, MLineComment i, acc)
      | _, (';' | ',' | '-' | '+' | '*' | '/' | '=' | '~') ->
          ( i + 1,
            MNone,
            { v = TIdentifier (Char.to_string char); s = (i, i + 1, ctx) }
            :: flush () )
      | _, ('(' | '[' | '{') ->
          (i + 1, MNone, { v = TLParen char; s = (i, i + 1, ctx) } :: flush ())
      | _, (')' | ']' | '}') ->
          (i + 1, MNone, { v = TRParen char; s = (i, i + 1, ctx) } :: flush ())
      | _, '"' -> (i + 1, MString (i + 1), flush ())
      | _, (' ' | '\n' | '\t' | '\r') -> (i + 1, MNone, flush ())
      | MIdent _, _ -> (i + 1, mode, acc)
      | MNone, _ -> (i + 1, MIdent i, acc)
  in
  List.rev @@ go (0, MNone, [])

type j_expr = string spanned [@@deriving sexp_of]

type j_statement =
  | Let of { dst : string spanned; src : j_expr }
  | Return of j_expr
[@@deriving sexp_of]

type j_field = { ftype : string spanned; name : string spanned }
[@@deriving sexp_of]

type metod_category = Method | Constructor [@@deriving sexp_of]

type j_method = {
  category : metod_category;
  mtype : string spanned;
  name : string spanned;
  params : j_field list;
  statements : j_statement list;
}
[@@deriving sexp_of]

type j_class = {
  name : string spanned;
  fields : j_field list;
  methods : j_method list;
}
[@@deriving sexp_of]

type j_file = { classes : j_class list } [@@deriving sexp_of]

let eof_span (a, b, ctx) = (b, b, ctx)

let err_expected eof tokens expected_msg =
  match tokens with
  | { s } :: _ -> err ("Expected " ^ expected_msg) s
  | [] -> err ("Expected " ^ expected_msg) eof

let expect_ident eof tokens expected_msg =
  match tokens with
  | { v = TIdentifier name; s } :: rest -> ({ v = name; s }, eof_span s, rest)
  | _ -> err_expected eof tokens expected_msg

let expect_x_ident eof tokens expected =
  let msg = "'" ^ expected ^ "'" in
  let sth, eof, rest = expect_ident eof tokens msg in
  if sth.v = expected then (sth, eof, rest) else err ("Expected " ^ msg) sth.s

let expect_lparen eof tokens paren =
  match tokens with
  | { v = TLParen p; s } :: rest when p = paren ->
      ({ v = p; s }, eof_span s, rest)
  | _ -> err_expected eof tokens ("'" ^ Char.to_string paren ^ "'")

let todo_ : span = (1, 1, { source = ""; filename = "" })

(*
class cwrr { field int iaa; method int iwrr (string sbb, int icc) { return ret; return wr; }  }
 *)

let parse_expr eof tokens =
  let what, eof, rest = expect_ident eof tokens "expression" in
  (what, eof, rest)

let parse_stmt eof tokens =
  match tokens with
  | { v = TKeyword "return"; s } :: rest ->
      let eof = eof_span s in
      let what, eof, rest = parse_expr eof rest in
      let _, eof, rest = expect_x_ident eof rest ";" in
      (Return what, eof, rest)
  | _ -> failwith "todo"

let parse_method eof tokens =
  match tokens with
  | { v = TKeyword (("constructor" | "method") as cat); s } :: rest ->
      let eof = eof_span s in
      let category =
        match cat with
        | "constructor" -> Constructor
        | "method" -> Method
        | _ -> failwith "unreachable"
      in
      let mtype, eof, rest = expect_ident eof rest "method type" in
      let name, eof, rest = expect_ident eof rest "method name" in
      (* params *)
      let rec go_params eof tokens acc =
        match tokens with
        | { v = TRParen ')' } :: rest -> (acc, eof, rest)
        | _ -> (
            let atype, eof, rest = expect_ident eof tokens "argument type" in
            let aname, eof, rest = expect_ident eof rest "argument name" in
            let acc = { name = aname; ftype = atype } :: acc in

            match rest with
            | { v = TRParen ')' } :: rest -> (acc, eof, rest)
            | { v = TIdentifier "," } :: rest -> go_params eof rest acc
            | _ -> err_expected eof rest "')'")
      in
      let _, eof, rest = expect_lparen eof rest '(' in
      let params, eof, rest = go_params eof rest [] in
      let params = List.rev params in
      (* body *)
      let rec go_body eof tokens acc =
        match tokens with
        | { v = TRParen '}' } :: rest -> (acc, eof, rest)
        | _ ->
            let stmt, eof, rest = parse_stmt eof tokens in
            go_body eof rest (stmt :: acc)
      in
      let _, eof, rest = expect_lparen eof rest '{' in
      let stmts, eof, rest = go_body eof rest [] in
      let stmts = List.rev stmts in

      ({ category; mtype; name; params; statements = stmts }, eof, rest)
  | _ -> failwith "unreachable"

let parse_field eof tokens =
  match tokens with
  | { v = TKeyword "field"; s } :: rest ->
      let eof = eof_span s in
      let ftype, eof, rest = expect_ident eof rest "field type" in
      let rec go eof tokens acc =
        let name, eof, rest = expect_ident eof tokens "field name" in
        let acc = { name; ftype } :: acc in

        match rest with
        | { v = TIdentifier ";" } :: rest -> (acc, eof, rest)
        | { v = TIdentifier "," } :: rest -> go eof rest acc
        | _ -> err_expected eof rest "';'"
      in
      go eof rest []
  | _ -> failwith "unreachable"

let parse_class eof tokens =
  let name, eof, rest = expect_ident eof tokens "class name" in
  let _, eof, rest = expect_lparen eof rest '{' in
  let rec go eof tokens cls =
    match tokens with
    | { v = TKeyword "field" } :: rest ->
        let item, eof, rest = parse_field eof tokens in
        go eof rest { cls with fields = item @ cls.fields }
    | { v = TKeyword "method" } :: rest ->
        let item, eof, rest = parse_method eof tokens in
        go eof rest { cls with methods = item :: cls.methods }
    | { v = TRParen '}' } :: rest -> (cls, eof, rest)
    | _ -> err_expected eof tokens "'}'"
  in
  let cls, eof, rest = go eof rest { name; fields = []; methods = [] } in
  ( { cls with fields = List.rev cls.fields; methods = List.rev cls.methods },
    eof,
    rest )

let parse_file tokens =
  let rec go (tokens, acc) =
    match tokens with
    | { v = TKeyword "class"; s } :: rest ->
        let c, _, rest = parse_class (eof_span s) rest in
        go (rest, c :: acc)
    | { s } :: rest -> err "unexpected token" s
    | [] -> List.rev acc
  in
  let classes = go (tokens, []) in
  { classes }

let compile_file file =
  (* In_channel.with_open_text name @@ fun input -> parse input symbol_tbl *)
  let str = In_channel.input_all stdin in
  let tokens = tokenize str file in
  (* let toks = List.map tokens ~f:(fun { v } -> v) in
  Stdio.print_s (sexp_of_tokens toks); *)
  let ast = parse_file tokens in
  Stdio.print_s (sexp_of_j_file ast)

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
