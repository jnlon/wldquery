Log.set_info [];;

let print_keys_exit () =
  print_endline "Valid keys are: ";
  List.iter (fun t -> Printf.printf "   %s\n" (fst t)) World.header_pairs;
  exit 1;;

let print_usage_exit () =
  print_endline "Usage: ./wldquery [wldpath] [key1 key2 ...]";
  print_keys_exit ();;

let wld_header_of_path path = 
  let in_ch = open_in_bin path in
  World.read_header in_ch ;;

let main () =  
  try begin
    let assoc = List.assoc in
    let inpath = Sys.argv.(1) in
    let keys = List.tl @@ List.tl @@ Array.to_list Sys.argv in
    let header = wld_header_of_path inpath in
    if (List.length keys) = 0 then raise Not_found else begin
      List.iter (fun key -> World.print_header_pair (key,(assoc key header))) keys
    end
  end
  with Invalid_argument i -> print_usage_exit ()
     | Not_found -> print_keys_exit ()
     | Unix.Unix_error (e,s1,s2) -> Printf.printf "%s : %s ('%s')" s1 (Unix.error_message e) s2 ;;

main () ;;
