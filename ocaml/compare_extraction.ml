open Utils
;;

let gen_list count e =
  let rec gen c l =
  if c <= 0 then l else gen (c-1) (e::l)
  in
  gen count []

let split_line min_count line =  
  let splits = Str.split (Str.regexp "\t") line in
  let len = List.length splits in 
  if len < min_count then
    splits @ gen_list (min_count-len) "0"
  else
    splits

let extraction_stream filename =
  let stream = file_stream filename in
  let header = Stream.next stream |> split_line 0 |> List.mapi (fun i s -> (s, i)) in
  (stream, header)

let filter_by_index w l =
  let rec f1 w l i =
    match w with
    | headw::tailw ->
      if headw < i then
        failwith "incorrect whitelist"
      else if headw > i then
        f1 w (List.tl l) (i+1)
      else begin
        (List.hd l)::(f1 tailw (List.tl l) (i+1))
        end
    | [] -> []
  in
  f1 w l 0

let get_record whitelist line =
  (* printfn "First: %s" (String.sub line 0 100); *)
  (* printfn "%d" (List.hd whitelist);            *)
  let header_len = (List.last whitelist) + 1 in
  let splits = split_line header_len line in
  let a = filter_by_index whitelist splits in
  a

let compare_stream whitelist1 whitelist2 stream1 stream2 =
  let rec f1 stream1 stream2 i =
    let r1 = Stream.next stream1 |> get_record whitelist1 in
    let r2 = Stream.next stream2 |> get_record whitelist2 in
    begin
      if r1 <> r2 then begin
        printfn "%d Left : %s" i (List.to_string identity r1); 
        printfn "%d Right: %s" i (List.to_string identity r2);
        failwith "Different records!";
        end
      else
        f1 stream1 stream2 (i+1)
    end
  in
  f1 stream1 stream2 0      

let compare col_pattern file1 file2 =
  let stream1, header1 = extraction_stream file1 in
  let stream2, header2 = extraction_stream file2 in
  let col_regex = Str.regexp col_pattern in
  let cols1 = List.filter (function
    | (name, i) -> not (Str.string_match col_regex name 0)
    ) header1 in
  let cols2 = List.filter (function
    | (name, i) -> not (Str.string_match col_regex name 0)
    ) header2 in
  let names1, indexes1 = List.split cols1 in
  let names2, indexes2 = List.split cols2 in
  if (names1) <> names2 then begin
    printfn "Left header : %s" (List.to_string identity names1); 
    printfn "Right header: %s" (List.to_string identity names2);
    failwith "wrong headers";
    end    
  else
    compare_stream indexes1 indexes2 stream1 stream2    
  
;;

match Sys.argv with
| [|_; file1; file2 |] ->
  compare "FuzzyCompleteMatch" file1 file2
| _ -> failwith (List.to_string identity (Array.to_list Sys.argv));;
