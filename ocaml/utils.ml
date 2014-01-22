(* Pipeline operator *)
let (|>) f g =
  g f

let identity a = a

let channel_stream in_channel =
  Stream.from (fun count ->
    try
      Some (input_line in_channel)
    with End_of_file ->
        None
  )
  
let file_stream filename = channel_stream (open_in filename)

let printfn format =
  Printf.kfprintf (fun ch -> output_char ch '\n') stdout format
    
let read_all in_stream =
  let buffer = Buffer.create 1024 in
    in_stream |> Stream.iter (fun l -> Buffer.add_string buffer l);
    Buffer.contents buffer

module StrMap = Map.Make(String)

module IntMap = Map.Make(Int32)

module List = struct
  include List
  
  let rec last list =
    match list with
    | head::[] -> head
    | head::tail -> last tail
    | [] -> invalid_arg "Empty list!"


  let to_string converter list =
    "[" ^ (List.map converter list |> String.concat "; ") ^ "]"
end
