open Utils
;;

let replace = ref false
let inverse = ref false
let fullText = ref false
let inputFile = ref ""
let patterns = ref []  
let usage = "Usage: ggrep [options] <in-pattern> [out-pattern]\nOptions:"
    
let rec specs = [
  ("-r", Arg.Set replace, "replace mode");
  ("-v", Arg.Set inverse, "inverse");
  ("-f", Arg.Set fullText, "process full text");
  ("-i", Arg.Set_string inputFile, "read from file nor stdin");
            ] |> Arg.align
;;
    
Arg.parse specs (fun s -> patterns := s::!patterns) usage;;

let error msg =
  begin
    Printf.fprintf stderr "%s\n" msg;
    Arg.usage specs usage;
    exit(1)
  end;;

let (inPattern, outPattern) = match !patterns with
  |[i; o] -> (i, o)
  |[i] -> (i, "\\0")
  |_ -> error "Incorrect argument numbers"
;;

let process regex line =
  if !replace then
    Str.global_replace regex outPattern line |> print_endline
  else
    try
      begin
        Str.search_forward regex line 0 |> ignore;
        Str.replace_matched outPattern line |> print_endline
      end
    with Not_found -> ()

let inChannel = (match !inputFile with
  | "" -> stdin
  | s -> open_in s) |> channel_stream
;;

if !fullText then
  inChannel |> read_all |> process (Str.regexp inPattern)
else
  inChannel |> Stream.iter (process (Str.regexp inPattern))
