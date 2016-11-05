(*Log.set_info [];;*)

exception Unsupported ;;

let print_keys_exit () =
  print_endline "Valid keys are: ";
  List.iter (fun t -> Printf.printf "   %s\n" (fst t)) World.wld_header_read_seq;
  exit 1;;

let print_usage_exit () =
  print_endline "Usage: ./wldquery [wldpath] [key1 key2 ...]";
  print_keys_exit ();;

let print_assoc_item seq_type b = 
  let open Util in
  let open World in
  begin
    match seq_type with
      | Bool -> print_string (string_of_bool @@ bool_of_bytes b)
      | Byte -> print_int @@ int_of_char (Bytes.get b 0) 
      | Int16 -> print_int @@ int16_of_bytes b
      | Int32 -> Printf.printf "%ld" @@ int32_of_bytes b
      | Int64 -> Printf.printf "%Ld" @@ int64_of_bytes b
      | Array (read_type_t, array_size_t) -> print_string "Warning: Printing array values not implemented"
      | String -> print_string @@ string_of_bytes b 
      | Single -> print_float @@ single_of_bytes b
      | Double -> Printf.printf "%Ld" @@ double_of_bytes b
    end;
    print_newline () ;;

let main () =  
  try begin
    let assoc = List.assoc in
    let inpath = Sys.argv.(1) in
    let keys = List.tl @@ List.tl @@ Array.to_list Sys.argv in
    let header = World.wld_header_of_path inpath in
    let header_def = World.wld_header_read_seq in
    if (List.length keys) = 0 then raise Not_found else begin
      (*List.iter print_endline keys;*)
      List.iter (fun key -> print_assoc_item (assoc key header_def) (assoc key header)) keys
    end
  end
  with Invalid_argument i -> print_usage_exit ()
     | Not_found -> print_keys_exit ()
     | Unix.Unix_error (e,s1,s2) -> Printf.printf "%s : %s ('%s')" s1 (Unix.error_message e) s2 ;;

main () ;;
