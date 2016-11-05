Log.set_info [];;

let tiles =  
  try 
    World.wld_header_of_path Sys.argv.(1)
  with Invalid_argument _ -> 
    World.wld_header_of_fd Unix.stdin 
;;
