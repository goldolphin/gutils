(* Pipeline operator *)
let (|>) f g =
  g f

let in_stream in_channel =
  Stream.from (fun count ->
    try
      Some (input_line in_channel)
    with End_of_file ->
        None
  )
  
let file_stream filename = in_stream (open_in filename)

let printfn format =
  Printf.kfprintf (fun ch -> output_char ch '\n') stdout format
