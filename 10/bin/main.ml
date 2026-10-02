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

let format_error kind (msg, (a, b, ctx)) =
  let { source; filename } = ctx in
  let rawlines = String.split ~on:'\n' source in
  let newline_locations =
    List.fold rawlines
      ~f:(fun acc x -> (List.hd_exn acc + String.length x + 1) :: acc)
      ~init:[ -1 ]
    |> List.rev |> List.tl_exn
  in
  let get_line_pos ln =
    if ln = 0 then 0 else List.nth_exn newline_locations (ln - 1) + 1
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
  Printf.sprintf "%s:%d:%d: %s: %s\n" filename (ln_a + 1) (char_a + 1) kind msg
  ^ String.concat (List.mapi lines ~f:fmt_line)

let err msg (span : span) =
  Stdio.prerr_endline (format_error "error" (msg, span));
  Stdlib.exit 1

let err_note msg m_span note n_span =
  Stdio.prerr_endline (format_error "error" (msg, m_span));
  Stdio.prerr_endline (format_error "note" (note, n_span));
  Stdlib.exit 1

type tok =
  | TLParen of char
  | TRParen of char
  | TSpecial of string
  | TIdentifier of string
  | TKeyword of string
  | TNumber of int
  | TString of string
[@@deriving sexp_of]

type tokens = tok list [@@deriving sexp_of]
type stok = tok spanned [@@deriving sexp_of]
type stoks = tok spanned list [@@deriving sexp_of]

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
  | "class" | "field" | "static" | "constructor" | "method" | "function" | "let"
  | "var" | "return" | "do" | "if" | "else" | "while" ->
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
      | _, '<' when next = '=' ->
          (i + 2, MNone, { v = TSpecial "<="; s = (i, i + 2, ctx) } :: flush ())
      | _, '>' when next = '=' ->
          (i + 2, MNone, { v = TSpecial ">="; s = (i, i + 2, ctx) } :: flush ())
      | ( _,
          (';' | ',' | '-' | '+' | '*' | '/' | '=' | '~' | '|' | '&' | '>' | '<')
        ) ->
          ( i + 1,
            MNone,
            { v = TSpecial (Char.to_string char); s = (i, i + 1, ctx) }
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

type jexpr =
  | ParenExpr of sjexpr
  | IdentifierExpr of string
  | StringExpr of string
  | NumberExpr of int
  | FnCallExpr of { fn : sjexpr; args : sjexpr list }
  | BinOpExpr of { op : string; left : sjexpr; right : sjexpr }
  | UnaryOpExpr of { op : string; arg : sjexpr }
[@@deriving sexp_of]

and sjexpr = jexpr spanned [@@deriving sexp_of]

type j_fields = { ftype : string spanned; names : string spanned list }
[@@deriving sexp_of]

type j_arg = { atype : string spanned; name : string spanned }
[@@deriving sexp_of]

type j_statement =
  | Let of { name : string spanned; value : sjexpr }
  | If of {
      cond : sjexpr;
      body : j_statement list;
      ielse : j_statement list option;
    }
  | While of { cond : sjexpr; body : j_statement list }
  | Do of sjexpr
  | Var of j_fields
  | Return of sjexpr option
[@@deriving sexp_of]

type metod_category = Method | Constructor | Function [@@deriving sexp_of]

type j_method = {
  category : metod_category;
  mtype : string spanned;
  name : string spanned;
  params : j_arg list;
  body : j_statement list;
}
[@@deriving sexp_of]

type j_class = {
  name : string spanned;
  fields : j_fields list;
  statics : j_fields list;
  methods : j_method list;
}
[@@deriving sexp_of]

type j_file = { classes : j_class list } [@@deriving sexp_of]

let eof_span (a, b, ctx) = (b, b, ctx)

let err_expected eof tokens expected_msg =
  match tokens with
  | { s } :: _ -> err ("Expected " ^ expected_msg) s
  | [] -> err ("Expected " ^ expected_msg) eof

let err_expected_note eof tokens expected_msg note n_span =
  match tokens with
  | { s } :: _ -> err_note ("Expected " ^ expected_msg) s note n_span
  | [] -> err_note ("Expected " ^ expected_msg) eof note n_span

let expect_ident eof tokens expected_msg =
  match tokens with
  | { v = TIdentifier name; s } :: rest -> ({ v = name; s }, eof_span s, rest)
  | _ -> err_expected eof tokens expected_msg

let expect_special eof tokens expected_msg =
  match tokens with
  | { v = TSpecial name; s } :: rest -> ({ v = name; s }, eof_span s, rest)
  | _ -> err_expected eof tokens expected_msg

let expect_x_special eof tokens expected =
  let msg = "'" ^ expected ^ "'" in
  let sth, eof, rest = expect_special eof tokens msg in
  if sth.v = expected then (sth, eof, rest) else err ("Expected " ^ msg) sth.s

let expect_lparen eof tokens paren =
  match tokens with
  | { v = TLParen p; s } :: rest when p = paren ->
      ({ v = p; s }, eof_span s, rest)
  | _ -> err_expected eof tokens ("'" ^ Char.to_string paren ^ "'")

let expect_rparen eof tokens paren s =
  match tokens with
  | { v = TRParen p; s } :: rest when p = paren ->
      ({ v = p; s }, eof_span s, rest)
  | _ ->
      err_expected_note eof tokens
        ("'" ^ Char.to_string paren ^ "'")
        "To match:" s

(* TODO: remove *)
let todo_span : span = (0, 0, { source = "todo_span"; filename = "todo_span" })

(*

class cwrr {
  field int iaa;
  constructor void incSize() {
    if (((y + size) < 254) & ((x + size) < 510)) {
      do erase();
      let size = size + 2;
      do draw();
    }
    return;
  }
}

class cwrr {
  field int iaa;
  method int iwrr (string sbb, int icc) { 
    return 1 | 2 & 3 | 4 > 1 | 2;
  }  
}
 *)

let span_from_to left right =
  let s_beg, _, ctx = left in
  let _, s_end, _ = right in
  (s_beg, s_end, ctx)

let rec parse_atom eof tokens : sjexpr * span * stoks =
  match tokens with
  | { v = TSpecial (("~" | "-") as op); s = beg_s } :: rest ->
      let eof = eof_span beg_s in
      let arg, eof, rest = parse_postfix eof rest in
      ({ v = UnaryOpExpr { op; arg }; s = span_from_to beg_s eof }, eof, rest)
  | { v = TLParen '('; s = lpar_s } :: rest ->
      let eof = eof_span lpar_s in
      let expr, eof, rest = parse_expr eof rest in
      let rpar, eof, rest = expect_rparen eof rest ')' lpar_s in
      ({ v = ParenExpr expr; s = span_from_to lpar_s rpar.s }, eof, rest)
  | { v = TIdentifier ident; s } :: rest ->
      ({ v = IdentifierExpr ident; s }, eof_span s, rest)
  | { v = TString ident; s } :: rest ->
      ({ v = StringExpr ident; s = eof_span s }, eof, rest)
  | { v = TNumber ident; s } :: rest ->
      ({ v = NumberExpr ident; s = eof_span s }, eof, rest)
  | _ -> err_expected eof tokens "atom"

and parse_postfix eof tokens =
  let next = parse_atom in
  let term0, eof, rest = next eof tokens in
  match rest with
  | { v = TLParen '('; s = lpar_s } :: rest ->
      let rec go_args eof tokens acc =
        match tokens with
        | { v = TRParen ')'; s } :: rest -> (List.rev acc, eof_span s, rest)
        | [] -> err_expected eof tokens "')'"
        | _ -> (
            let arg, eof, rest = parse_expr eof tokens in
            let acc = arg :: acc in

            match rest with
            | { v = TRParen ')'; s } :: rest -> (List.rev acc, eof_span s, rest)
            | { v = TSpecial ","; s } :: rest -> go_args (eof_span s) rest acc
            | _ -> err_expected eof rest "')'")
      in
      let args, eof, rest = go_args eof rest [] in
      let s = span_from_to term0.s eof in
      ({ v = FnCallExpr { fn = term0; args }; s }, eof, rest)
  | _ -> (term0, eof, rest)

and parse_mult eof tokens =
  let next = parse_postfix in
  let this = parse_mult in
  let left, eof, rest = next eof tokens in
  match rest with
  | { v = TSpecial (("*" | "/" | "&") as op) } :: rest ->
      let right, eof, rest = this eof rest in
      ( { v = BinOpExpr { op; left; right }; s = span_from_to left.s right.s },
        eof,
        rest )
  | _ -> (left, eof, rest)

and parse_sum eof tokens =
  let next = parse_mult in
  let this = parse_sum in
  let left, eof, rest = next eof tokens in
  match rest with
  | { v = TSpecial (("+" | "-" | "|") as op) } :: rest ->
      let right, eof, rest = this eof rest in
      ( { v = BinOpExpr { op; left; right }; s = span_from_to left.s right.s },
        eof,
        rest )
  | _ -> (left, eof, rest)

and parse_cond eof tokens =
  let next = parse_sum in
  let this = parse_cond in
  let left, eof, rest = next eof tokens in
  match rest with
  | { v = TSpecial (("<" | "<=" | "=" | ">=" | ">") as op) } :: rest ->
      let right, eof, rest = this eof rest in
      ( { v = BinOpExpr { op; left; right }; s = span_from_to left.s right.s },
        eof,
        rest )
  | _ -> (left, eof, rest)

and parse_expr eof tokens : sjexpr * span * stoks =
  let next = parse_cond in
  let term0, eof, rest = next eof tokens in
  match rest with _ -> (term0, eof, rest)

and parse_block eof tokens =
  let rec go eof tokens acc =
    match tokens with
    | { v = TRParen '}'; s } :: rest -> (List.rev acc, eof_span s, rest)
    | [] -> err_expected eof tokens "'}'"
    | _ ->
        let stmt, eof, rest = parse_stmt eof tokens in
        go eof rest (stmt :: acc)
  in
  let _, eof, rest = expect_lparen eof tokens '{' in
  go eof rest []

and parse_stmt eof tokens =
  match tokens with
  | { v = TKeyword "return"; s } :: rest -> (
      let eof = eof_span s in
      match rest with
      | { v = TSpecial ";"; s } :: rest -> (Return None, eof_span s, rest)
      | _ ->
          let what, eof, rest = parse_expr eof rest in
          let _, eof, rest = expect_x_special eof rest ";" in
          (Return (Some what), eof, rest))
  | { v = TKeyword "var" } :: rest ->
      let fields, eof, rest = parse_field eof tokens "var" in
      (Var fields, eof, rest)
  | { v = TKeyword "do"; s } :: rest ->
      let eof = eof_span s in
      let what, eof, rest = parse_expr eof rest in
      let _, eof, rest = expect_x_special eof rest ";" in
      (Do what, eof, rest)
  | { v = TKeyword "let"; s } :: rest ->
      let eof = eof_span s in
      let name, eof, rest = expect_ident eof rest "name" in
      let _, eof, rest = expect_x_special eof rest "=" in
      let value, eof, rest = parse_expr eof rest in
      let _, eof, rest = expect_x_special eof rest ";" in
      (Let { name; value }, eof, rest)
  | { v = TKeyword "if"; s } :: rest ->
      let eof = eof_span s in
      let lpar, eof, rest = expect_lparen eof rest '(' in
      let cond, eof, rest = parse_expr eof rest in
      let _, eof, rest = expect_rparen eof rest ')' lpar.s in
      let body, eof, rest = parse_block eof rest in
      let go eof tokens =
        match tokens with
        | { v = TKeyword "else"; s } :: rest ->
            let eof = eof_span s in
            let body, eof, rest = parse_block eof rest in
            (Some body, eof, rest)
        | _ -> (None, eof, tokens)
      in
      let ielse, eof, rest = go eof rest in
      (* TODO: if else *)
      (If { cond; body; ielse }, eof, rest)
  | { v = TKeyword "while"; s } :: rest ->
      let eof = eof_span s in
      let lpar, eof, rest = expect_lparen eof rest '(' in
      let cond, eof, rest = parse_expr eof rest in
      let _, eof, rest = expect_rparen eof rest ')' lpar.s in
      let body, eof, rest = parse_block eof rest in
      (While { cond; body }, eof, rest)
  | _ -> err_expected eof tokens "statement"

and parse_field eof tokens keyword : j_fields * span * stoks =
  match tokens with
  | { v = TKeyword kw; s } :: rest when kw = keyword ->
      let eof = eof_span s in
      let ftype, eof, rest = expect_ident eof rest (kw ^ " type") in
      let rec go eof tokens acc =
        let name, eof, rest = expect_ident eof tokens (kw ^ " name") in
        let acc = name :: acc in

        match rest with
        | { v = TSpecial ";"; s } :: rest -> (acc, eof, rest)
        | { v = TSpecial ","; s } :: rest -> go eof rest acc
        | _ -> err_expected eof rest "';'"
      in
      let names, eof, rest = go eof rest [] in
      ({ names; ftype }, eof, rest)
  | _ -> failwith "unreachable"

let parse_method eof tokens =
  match tokens with
  | { v = TKeyword (("constructor" | "method" | "function") as cat); s } :: rest
    ->
      let eof = eof_span s in
      let category =
        match cat with
        | "constructor" -> Constructor
        | "method" -> Method
        | "function" -> Function
        | _ -> failwith "unreachable"
      in
      let mtype, eof, rest = expect_ident eof rest "method type" in
      let name, eof, rest = expect_ident eof rest "method name" in
      (* params *)
      let rec go_params eof tokens acc =
        match tokens with
        | { v = TRParen ')'; s } :: rest -> (List.rev acc, eof_span s, rest)
        | [] -> err_expected eof tokens "')'"
        | _ -> (
            let ptype, eof, rest = expect_ident eof tokens "param type" in
            let pname, eof, rest = expect_ident eof rest "param name" in
            let acc = { name = pname; atype = ptype } :: acc in

            match rest with
            | { v = TRParen ')'; s } :: rest -> (List.rev acc, eof_span s, rest)
            | { v = TSpecial ","; s } :: rest -> go_params (eof_span s) rest acc
            | _ -> err_expected eof rest "')'")
      in
      let _, eof, rest = expect_lparen eof rest '(' in
      let params, eof, rest = go_params eof rest [] in
      (* body *)
      let stmts, eof, rest = parse_block eof rest in

      ({ category; mtype; name; params; body = stmts }, eof, rest)
  | _ -> failwith "unreachable"

let parse_class eof tokens =
  let name, eof, rest = expect_ident eof tokens "class name" in
  let _, eof, rest = expect_lparen eof rest '{' in
  let rec go eof tokens cls =
    match tokens with
    | { v = TKeyword "field" } :: rest ->
        let item, eof, rest = parse_field eof tokens "field" in
        go eof rest { cls with fields = item :: cls.fields }
    | { v = TKeyword "static" } :: rest ->
        let item, eof, rest = parse_field eof tokens "static" in
        go eof rest { cls with statics = item :: cls.statics }
    | { v = TKeyword ("method" | "constructor" | "function") } :: rest ->
        let item, eof, rest = parse_method eof tokens in
        go eof rest { cls with methods = item :: cls.methods }
    | { v = TRParen '}' } :: rest -> (cls, eof, rest)
    | _ -> err_expected eof tokens "'}'"
  in
  let cls, eof, rest =
    go eof rest { name; fields = []; statics = []; methods = [] }
  in
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
