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
;;

in_stream stdin |> Stream.iter (fun line ->
  split_line 0 line |> List.length |>
  printfn "%d" )
