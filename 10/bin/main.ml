open Base
open Jackc.Parser

let write_lines out lines =
  List.iter lines ~f:(fun line ->
      Out_channel.output_string out line;
      Out_channel.output_char out '\n')

let compile_file ast out =
  Out_channel.output_string out "wrr";
  Stdio.print_s (sexp_of_j_file ast)

let process_file file =
  let ast = parse_file file in
  let outname = Stdlib.Filename.remove_extension file ^ ".jackvm" in
  let tmpname = outname ^ ".tmp" in
  let out = Out_channel.open_text tmpname in
  try
    compile_file ast out;
    Out_channel.close out;
    Stdlib.Sys.rename tmpname outname
  with exn ->
    Out_channel.close out;
    (try Stdlib.Sys.remove file with _ -> ());
    raise exn

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then Stdio.prerr_endline "Specify input file(s)!"
  else
    let res =
      argv |> Array.to_sequence_mutable
      |> Fn.flip Sequence.drop_eagerly 1
      |> Sequence.iter ~f:process_file
      (* |> Sequence.to_list |> Result.all *)
    in
    res
